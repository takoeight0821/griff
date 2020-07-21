{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.TypeRep where

import Language.Griff.Id
import Language.Griff.Prelude
import Language.Griff.Pretty
import qualified Text.PrettyPrint.HughesPJ as P

----------------------
-- Kind and HasKind --
----------------------

data Kind = Star | KArr Kind Kind
  deriving stock (Eq, Ord, Show)

class HasKind a where
  kind :: a -> Kind

instance HasKind a => HasKind (Id a) where
  kind = kind . view idMeta

instance HasKind Kind where
  kind = id

instance Pretty Kind where
  pPrintPrec _ _ Star = "*"
  pPrintPrec l d (KArr k1 k2) = P.maybeParens (d > 10) $ pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2

----------
-- Type --
----------

data Scheme = Forall [TyVar] Type
  deriving stock (Eq, Show, Ord)

instance HasKind Scheme where
  kind (Forall _ t) = kind t

instance Pretty Scheme where
  pPrint (Forall vs t) = "forall" <+> P.sep (map pPrint vs) <> "." <+> pPrint t

data Type
  = TyApp Type Type
  | TyVar TyVar
  | TyCon (Id Kind)
  | PrimT PrimT
  | TyArr Type Type
  | TupleT [Type]
  | LazyT Type
  deriving stock (Eq, Show, Ord)

instance HasKind Type where
  kind (TyApp t _) = case kind t of
    (KArr _ k) -> k
    _ -> error "invalid kind"
  kind (TyVar t) = kind t
  kind (TyCon c) = kind c
  kind (PrimT _) = Star
  kind (TyArr _ _) = Star
  kind (TupleT _) = Star
  kind (LazyT _) = Star

instance Pretty Type where
  pPrintPrec l d (TyApp t1 t2) = P.maybeParens (d > 10) $ P.sep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVar v) = pPrint v
  pPrintPrec _ _ (TyCon c) = pPrint c
  pPrintPrec _ _ (PrimT p) = pPrint p
  pPrintPrec l d (TyArr t1 t2) = P.maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TupleT ts) = P.parens $ P.sep $ P.punctuate "," $ map pPrint ts
  pPrintPrec _ _ (LazyT t) = P.braces $ pPrint t

-------------------
-- Type variable --
-------------------

type TyVar = Id Kind

---------------------
-- Primitive Types --
---------------------

data PrimT = Int32T | Int64T | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord)

instance Pretty PrimT where
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"

------------------
-- Substitution --
------------------

-- Replace the specified quantified type variables by given meta type variables
substTy :: [TyVar] -> [Type] -> Type -> Type
substTy tvs tys ty = subst_ty (zip tvs tys) ty

subst_ty :: [(TyVar, Type)] -> Type -> Type
subst_ty env (TyApp t1 t2) = TyApp (subst_ty env t1) (subst_ty env t2)
subst_ty env (TyVar n) = fromMaybe (TyVar n) (lookup n env)
subst_ty env (TyArr t1 t2) = TyArr (subst_ty env t1) (subst_ty env t2)
subst_ty env (TupleT ts) = TupleT $ map (subst_ty env) ts
subst_ty env (LazyT t) = LazyT $ subst_ty env t
subst_ty _ t = t
