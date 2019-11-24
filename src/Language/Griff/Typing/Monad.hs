{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Language.Griff.Typing.Monad
  ( TypeError(..)
  , Constraint
  , Env
  , ConMap(..)
  , InferEff
  , addScheme
  , instantiate
  , generalize
  , runSolve
  , runInfer
  , closeOver
  , lookup
  , newMeta
  , expandTCon
  ) where

import           Control.Carrier.Error.Either
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.State.Strict
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
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

type Env = Map Id Scheme

newtype ConMap = ConMap { unConMap :: Map Id ([Id], Ty) }
  deriving (Eq, Show, Generic, Semigroup, Monoid)

instance PrettyVal ConMap

type InferEff sig m = (Has (Error TypeError) sig m, Has (State Env) sig m, Has (State ConMap) sig m, Has Fresh sig m)

lookup :: (Has (State Env) sig m, Has (Error TypeError) sig m, Has Fresh sig m) => Id -> m Ty
lookup x = do
  env <- get
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> instantiate s

newMeta :: Has Fresh sig m => m Ty
newMeta = do
  i <- newId "meta"
  return $ TVar i

addScheme :: Has (State Env) sig m => (Id, Scheme) -> m ()
addScheme (x, sc) = do
  let scope e = Map.insert x sc $ Map.delete x e
  modify scope

instantiate :: Has Fresh sig m => Scheme -> m Ty
instantiate (Forall as t) = do
  as' <- mapM (const newMeta) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: Env -> Ty -> Scheme
generalize env t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

runSolve :: Has (Error TypeError) sig m => [Constraint] -> m Subst
runSolve cs = solver st
  where st = (mempty, cs)

solver :: Has (Error TypeError) sig m => (Subst, [Constraint]) -> m Subst
solver (su, cs) = case cs of
  [] -> return su
  ((t1, t2) : cs0) -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, apply su1 cs0)

unifies :: Has (Error TypeError) sig m => Ty -> Ty -> m Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TRecord xs) (TRecord ys)
  | Map.keys xs == Map.keys ys = unifyMany (Map.elems xs) (Map.elems ys)
unifies (TVariant xs) (TVariant ys)
  | Map.keys xs == Map.keys ys = unifyMany (Map.elems xs) (Map.elems ys)
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: Has (Error TypeError) sig m => [Ty] -> [Ty] -> m Subst
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst $ fmap (apply (Subst s1)) s2 `Map.union` s1

bind :: Has (Error TypeError) sig m => Id -> Ty -> m Subst
bind a t | t == TVar a = return mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ Subst $ Map.singleton a t

occursCheck :: Substitutable a => Id -> a -> Bool
occursCheck a t = a `Set.member` ftv t

runInfer :: Functor m => Env -> StateC ConMap (StateC Env (ErrorC TypeError m)) a -> m (Either TypeError (Env, a))
runInfer env m = runError $ runState env $ evalState mempty m

closeOver :: Ty -> Scheme
closeOver = generalize mempty

expandTCon :: (Has (Error TypeError) sig m, Has (State ConMap) sig m) => Ty -> m Ty
expandTCon (TCon con args) = do
  env <- get
  case Map.lookup con $ unConMap env of
    Nothing -> throwError $ UndefinedType con
    Just (params, ty) ->
      pure $ apply (Subst $ Map.fromList $ zip params args) ty
expandTCon t = pure t
