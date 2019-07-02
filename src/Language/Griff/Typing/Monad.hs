{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Griff.Typing.Monad
  ( TypeError
  , Constraint
  , MonadInfer(..)
  , addScheme
  , instantiate
  , generalize
  , runSolve
  , runInfer
  , closeOver
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Language.Griff.Id
import           Language.Griff.Monad
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Env
import           Language.Griff.Typing.Subst
import           Prelude                     hiding (lookup)

data TypeError = UnificationFail Ty Ty
               | InfiniteType Id Ty
               | UnboundVariable Id
               | Ambigious [Constraint]
               | UnificationMismatch [Ty] [Ty]
  deriving (Show)

type Constraint = (Ty, Ty)

class (Monad m, MonadError TypeError m) => MonadInfer m where
  lookup :: Id -> m Ty
  update :: (Env -> Env) -> m ()
  getEnv :: m Env
  fresh :: m Ty

addScheme :: MonadInfer m => (Id, Scheme) -> m ()
addScheme (x, sc) = do
  let scope e = remove e x `extend` (x, sc)
  update scope

instantiate :: MonadInfer m => Scheme -> m Ty
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: Env -> Ty -> Scheme
generalize env t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

runSolve :: MonadError TypeError m => [(Ty, Ty)] -> m Subst
runSolve cs = solver st
  where st = (mempty, cs)

solver :: MonadError TypeError m => (Subst, [(Ty, Ty)]) -> m Subst
solver (su, cs) = case cs of
  [] -> return su
  ((t1, t2) : cs0) -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, apply su1 cs0)

unifies :: MonadError TypeError m => Ty -> Ty -> m Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TRecord xs) (TRecord ys)
  | Map.keys xs == Map.keys ys = unifyMany (Map.elems xs) (Map.elems ys)
unifies (TVariant xs) (TVariant ys)
  | Map.keys xs == Map.keys ys = unifyMany (Map.elems xs) (Map.elems ys)
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: MonadError TypeError m => [Ty] -> [Ty] -> m Subst
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst $ fmap (apply (Subst s1)) s2 `Map.union` s1

bind :: MonadError TypeError m => Id -> Ty -> m Subst
bind a t | t == TVar a = return mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ Subst $ Map.singleton a t

occursCheck :: Substitutable a => Id -> a -> Bool
occursCheck a t = a `Set.member` ftv t

newtype Infer m a = Infer (StateT Env (ExceptT TypeError (GriffT m)) a)
   deriving (Functor, Applicative, Monad, MonadError TypeError)

instance MonadIO m => MonadInfer (Infer m) where
  lookup x = do
    Env env <- Infer get
    case Map.lookup x env of
      Nothing -> throwError $ UnboundVariable x
      Just s  -> instantiate s
  update f = Infer $ modify f
  getEnv = Infer get
  fresh = Infer $ lift $ lift $ do
    i <- newId "meta"
    return $ TVar i

runInfer :: Env -> Infer m a -> GriffT m (Either TypeError (a, Env))
runInfer env (Infer m) = runExceptT $ runStateT m env

closeOver :: Ty -> Scheme
closeOver = generalize mempty
