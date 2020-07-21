{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Typing where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Griff.MonadUniq
import Language.Griff.Id
import Language.Griff.Prelude
import Language.Griff.Pretty
import Language.Griff.Syntax hiding (Type (..))
import qualified Language.Griff.Syntax as S
import Language.Griff.TcEnv
import Language.Griff.TypeRep
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.PrettyPrint as P

-------------------
-- Substituation --
-------------------

newtype Subst = Subst {unwrapSubst :: Map TyVar Type}
  deriving stock (Eq, Show)
  deriving newtype (Substitutable, Monoid)

instance Ixed Subst

instance At Subst where
  at k f (Subst m) = Subst <$> Map.alterF f k m

type instance Index Subst = TyVar

type instance IxValue Subst = Type

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst $ apply (Subst s1) s2 <> s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set TyVar

instance Substitutable Scheme where
  apply s (Forall ts t) = Forall ts $ apply s' t where s' = Subst $ foldr sans (unwrapSubst s) ts
  ftv (Forall ts t) = ftv t Set.\\ Set.fromList ts

instance Substitutable Type where
  apply s (TyApp t1 t2) = TyApp (apply s t1) (apply s t2)
  apply s (TyVar v) = fromMaybe (TyVar v) $ s ^. at v
  apply _ (TyCon c) = TyCon c
  apply _ (PrimT p) = PrimT p
  apply s (TyArr t1 t2) = TyArr (apply s t1) (apply s t2)
  apply s (TupleT ts) = TupleT $ map (apply s) ts
  apply s (LazyT t) = LazyT $ apply s t
  ftv (TyApp t1 t2) = ftv t1 <> ftv t2
  ftv (TyVar v) = Set.singleton v
  ftv (TyCon _) = mempty
  ftv (PrimT _) = mempty
  ftv (TyArr t1 t2) = ftv t1 <> ftv t2
  ftv (TupleT ts) = mconcat $ map ftv ts
  ftv (LazyT t) = ftv t

instance (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
  apply = fmap . apply
  ftv = foldMap ftv

----------------
-- Constraint --
----------------

data Constraint = Type :~ Type
  deriving stock (Eq, Show)

instance Substitutable Constraint where
  apply s (t1 :~ t2) = apply s t1 :~ apply s t2
  ftv (t1 :~ t2) = ftv t1 <> ftv t2

data WithPos = WithPos Constraint SourcePos
  deriving stock (Eq, Show)

instance Substitutable WithPos where
  apply s (WithPos c pos) = WithPos (apply s c) pos
  ftv (WithPos c _) = ftv c

solve :: [WithPos] -> Either (SourcePos, Doc) Subst
solve cs = solver (mempty, cs)

solver :: (Subst, [WithPos]) -> Either (SourcePos, Doc) Subst
solver (su, []) = pure su
solver (su, (WithPos (t1 :~ t2) pos) : cs) = do
  if kind t1 /= kind t2
    then Left (pos, "Kind mismatch:" <+> P.vcat [pPrint t1, pPrint t2])
    else do
      su1 <- unify pos t1 t2
      solver (su1 <> su, apply su1 cs)

unify :: SourcePos -> Type -> Type -> Either (SourcePos, Doc) Subst
unify pos (TyVar v) t = bind pos v t
unify pos t (TyVar v) = bind pos v t
unify pos (TyApp t11 t12) (TyApp t21 t22) = (<>) <$> unify pos t11 t21 <*> unify pos t12 t22
unify _ (TyCon c1) (TyCon c2) | c1 == c2 = pure mempty
unify _ (PrimT p1) (PrimT p2) | p1 == p2 = pure mempty
unify pos (TyArr t11 t12) (TyArr t21 t22) = (<>) <$> unify pos t11 t21 <*> unify pos t12 t22
unify pos (TupleT ts1) (TupleT ts2) = mconcat <$> zipWithM (unify pos) ts1 ts2
unify pos (LazyT t1) (LazyT t2) = unify pos t1 t2
unify pos t1 t2 = Left (pos, "Cannot unify types:" <+> P.vcat [pPrint t1, pPrint t2])

bind :: SourcePos -> TyVar -> Type -> Either (SourcePos, Doc) Subst
bind pos v t
  | t == TyVar v = pure mempty
  | occursCheck v t = Left (pos, "Occurs check for" <+> P.quotes (pPrint v) <+> "in:" <+> pPrint t)
  | otherwise = pure $ Subst $ Map.singleton v t

occursCheck :: Substitutable a => Id Kind -> a -> Bool
occursCheck v t = ftv t ^. contains v

--------------------------------
-- Instantiate and Generalize --
--------------------------------

instantiate :: MonadUniq m => Scheme -> m Type
instantiate (Forall vs t) = do
  ts <- traverse (\v -> newTyVar (kind v) (v ^. idName)) vs
  pure $ apply (Subst $ Map.fromList $ zip vs ts) t

generalize :: Substitutable a => a -> Type -> Scheme
generalize env t = Forall (toList $ ftv t Set.\\ ftv env) t

newTyVar :: MonadUniq f => Kind -> Name -> f Type
newTyVar k n = TyVar <$> newId k n

-----------------------------------------
-- Lookup and Extend type envirionment --
-----------------------------------------

lookupVar :: MonadReader TcEnv m => SourcePos -> Id NoMeta -> m Scheme
lookupVar pos x = do
  env <- view varEnv
  case env ^. at x of
    Just x' -> pure x'
    Nothing -> errorOn pos $ "Not in scope:" <+> P.quotes (pPrint x)

letVar :: (MonadReader TcEnv m, MonadUniq m) => Id NoMeta -> Type -> [WithPos] -> m a -> m a
letVar var ty cs m = do
  env <- view varEnv
  case solve cs of
    Right subst -> do
      defineVar var (generalize (apply subst env) (apply subst ty)) $
        local (over varEnv (apply subst)) m
    Left (pos, doc) -> errorOn pos doc

defineVar :: (MonadUniq m, MonadReader TcEnv m) => Id NoMeta -> Scheme -> m a -> m a
defineVar var sc m = do
  local (over varEnv (Map.insert var sc)) m
