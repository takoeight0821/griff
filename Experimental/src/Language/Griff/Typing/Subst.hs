{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Griff.Typing.Subst where

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Language.Griff.Id
import           Language.Griff.TypeRep

newtype Subst = Subst (Map Id Ty)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set Id

instance Substitutable Ty where
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2)     = apply s t1 `TArr` apply s t2
  apply _ (TPrim a)          = TPrim a
  apply s (TRecord xs)       = TRecord $ fmap (apply s) xs
  apply s (TVariant xs)      = TVariant $ fmap (apply s) xs

  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (TPrim _)       = Set.empty
  ftv (TRecord xs)   = Set.unions $ map ftv $ Map.elems xs
  ftv (TVariant xs)  = Set.unions $ map ftv $ Map.elems xs

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty
