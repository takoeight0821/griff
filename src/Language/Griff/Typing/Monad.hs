{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Griff.Typing.Monad
  ( TypeError(..)
  , Constraint
  , Env
  , InferEff
  , addScheme
  , instantiate
  , generalize
  , runSolve
  , runInfer
  , closeOver
  , lookup
  , fresh
  ) where

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.State
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           GHC.Generics
import           Language.Griff.Id
import           Language.Griff.Prelude
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Subst
import           Text.Show.Pretty

data TypeError = UnificationFail Ty Ty
               | InfiniteType Id Ty
               | UnboundVariable Id
               | UnificationMismatch [Ty] [Ty]
               | UndefinedType Id
               | UndecidableProj Text
  deriving (Show, Generic)

instance PrettyVal TypeError

type Constraint = (Ty, Ty)

type Env = Map.Map Id Scheme

type InferEff sig = (Member (Error TypeError) sig, Member (State Env) sig, Member Fresh sig)

lookup :: (Carrier sig m, InferEff sig) => Id -> m Ty
lookup x = do
  env <- get
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> instantiate s

fresh :: (Carrier sig m, InferEff sig) => m Ty
fresh = do
  i <- newId "meta"
  return $ TVar i

addScheme :: (Carrier sig m, InferEff sig) => (Id, Scheme) -> m ()
addScheme (x, sc) = do
  let scope e = Map.insert x sc $ Map.delete x e
  modify scope

instantiate :: (Carrier sig m, InferEff sig) => Scheme -> m Ty
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: Env -> Ty -> Scheme
generalize env t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

runSolve :: (Carrier sig m, InferEff sig) => [(Ty, Ty)] -> m Subst
runSolve cs = solver st
  where st = (mempty, cs)

solver :: (Carrier sig m, InferEff sig) => (Subst, [(Ty, Ty)]) -> m Subst
solver (su, cs) = case cs of
  [] -> return su
  ((t1, t2) : cs0) -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, apply su1 cs0)

unifies :: (Carrier sig m, InferEff sig) => Ty -> Ty -> m Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TRecord xs) (TRecord ys)
  | Map.keys xs == Map.keys ys = unifyMany (Map.elems xs) (Map.elems ys)
unifies (TVariant xs) (TVariant ys)
  | Map.keys xs == Map.keys ys = unifyMany (Map.elems xs) (Map.elems ys)
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: (Carrier sig m, InferEff sig) => [Ty] -> [Ty] -> m Subst
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst $ fmap (apply (Subst s1)) s2 `Map.union` s1

bind :: (Carrier sig m, InferEff sig) => Id -> Ty -> m Subst
bind a t | t == TVar a = return mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ Subst $ Map.singleton a t

occursCheck :: Substitutable a => Id -> a -> Bool
occursCheck a t = a `Set.member` ftv t

runInfer :: Env -> StateC Env (ErrorC TypeError m) a -> m (Either TypeError (Env, a))
runInfer env m = runError $ runState env m

closeOver :: Ty -> Scheme
closeOver = generalize mempty
