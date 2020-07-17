{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.TypeRep where

import Language.Griff.Id
import Language.Griff.Prelude
import Language.Griff.Pretty
import qualified Text.PrettyPrint.HughesPJ as P
import Text.Megaparsec.Pos (SourcePos)
import Data.List ((\\))

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

checkKinded :: (HasKind a, HasKind b) => a -> b -> Bool
checkKinded x y = kind x == kind y

instance Pretty Kind where
  pPrintPrec _ _ Star = "*"
  pPrintPrec l d (KArr k1 k2) = P.maybeParens (d > 10) $ pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2

----------
-- Type --
----------

type Sigma = Type
type Rho = Type -- No top-level Forall
type Tau = Type -- No Foralls anywhere

data Type
  = Forall [TyVar] Rho
  | TyApp Rho Rho
  | TyVar TyVar
  | TyCon (Id Kind)
  | PrimT PrimT
  | TyArr Type Type
  | TupleT [Rho]
  | LazyT Rho
  | MetaTv TyMeta
  deriving stock (Eq, Show, Ord)

instance HasKind Type where
  kind (Forall _ t) = kind t
  kind (TyApp t _) = case kind t of
    (KArr _ k) -> k
    _ -> error "invalid kind"
  kind (TyVar t) = kind t
  kind (TyCon c) = kind c
  kind (PrimT _) = Star
  kind (TyArr _ _) = Star
  kind (TupleT _) = Star
  kind (LazyT _) = Star
  kind (MetaTv tv) = kind tv

instance Pretty Type where
  pPrintPrec _ d (Forall vs t) = P.maybeParens (d > 9)  $ "forall" <+> P.sep (map pPrint vs) <> "." <+> pPrint t
  pPrintPrec l d (TyApp t1 t2) = P.maybeParens (d > 10) $ P.sep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVar v) = pPrint v
  pPrintPrec _ _ (TyCon c) = pPrint c
  pPrintPrec _ _ (PrimT p) = pPrint p
  pPrintPrec l d (TyArr t1 t2) = P.maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TupleT ts) = P.parens $ P.sep $ P.punctuate "," $ map pPrint ts
  pPrintPrec _ _ (LazyT t) = P.braces $ pPrint t
  pPrintPrec _ _ (MetaTv m) = pPrint m

-------------------
-- Type variable --
-------------------

data TyVar
  = BoundTv (Id Kind)
  | SkolemTv (Id Kind)
  deriving stock (Eq, Show, Ord)

tyVarName :: TyVar -> Name
tyVarName (BoundTv x) = x ^. idName
tyVarName (SkolemTv x) = x ^. idName

instance HasKind TyVar where
  kind (BoundTv x) = kind x
  kind (SkolemTv x) = kind x

instance Pretty TyVar where
  pPrint (BoundTv x) = pPrint x
  pPrint (SkolemTv x) = "_" <> pPrint x

---------------------
-- Primitive Types --
---------------------

data PrimT = IntT | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord)

instance Pretty PrimT where
  pPrint IntT = "Int#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"

-------------------
-- Meta variable --
-------------------

data TyMeta = TyMeta (Id Kind) TyRef

type TyRef = IORef (Maybe Tau)

instance HasKind TyMeta where
  kind (TyMeta x _) = kind x

instance Eq TyMeta where
  (TyMeta u1 _) == (TyMeta u2 _) = u1 == u2

instance Ord TyMeta where
  compare (TyMeta u1 _) (TyMeta u2 _) = compare u1 u2

instance Show TyMeta where
  show (TyMeta u _) = "TyMeta " <> show u

instance Pretty TyMeta where
  pPrint (TyMeta u _) = pPrint u

------------------
-- Substitution --
------------------

substTy :: SourcePos -> [TyVar] -> [Type] -> Rho -> Rho
substTy pos tvs tys ty
  | and (zipWith checkKinded tvs tys) = subst_ty (zip tvs tys) ty
  | otherwise = errorOn pos $ "Kind mismatch:" <+> pPrint tvs <> "," <+> pPrint tys

subst_ty :: [(TyVar, Type)] -> Rho -> Rho
subst_ty env (Forall ns rho) = Forall ns (subst_ty env' rho)
  where
    env' = [(n, ty') | (n, ty') <- env, not (n `elem` ns)]
subst_ty env (TyApp t1 t2) = TyApp (subst_ty env t1) (subst_ty env t2)
subst_ty env (TyVar n) = fromMaybe (TyVar n) $ lookup n env
subst_ty _ (TyCon tc) = TyCon tc
subst_ty _ (PrimT p) = PrimT p
subst_ty env (TyArr t1 t2) = TyArr (subst_ty env t1) (subst_ty env t2)
subst_ty env (TupleT ts) = TupleT (map (subst_ty env) ts)
subst_ty env (LazyT t) = LazyT $ subst_ty env t
subst_ty _ (MetaTv tv) = MetaTv tv

------------------------------
-- Free and bound variables --
------------------------------

metaTvs :: [Type] -> [TyMeta]
metaTvs tys = ordNub $ concatMap meta_tvs tys
  where
    meta_tvs (Forall _ t) = meta_tvs t
    meta_tvs (TyApp t1 t2) = meta_tvs t1 <> meta_tvs t2
    meta_tvs (TyVar _) = []
    meta_tvs (TyCon _) = []
    meta_tvs (PrimT _) = []
    meta_tvs (TyArr t1 t2) = meta_tvs t1 <> meta_tvs t2
    meta_tvs (TupleT ts) = concatMap meta_tvs ts
    meta_tvs (LazyT t) = meta_tvs t
    meta_tvs (MetaTv tv) = [tv]

freeTyVars :: [Type] -> [TyVar]
freeTyVars tys = ordNub $ concatMap free_tyVars tys
  where
    free_tyVars (Forall tvs ty) = free_tyVars ty \\ tvs
    free_tyVars (TyApp t1 t2) = free_tyVars t1 <> free_tyVars t2
    free_tyVars (TyVar t) = [t]
    free_tyVars (TyCon _) = []
    free_tyVars (PrimT _) = []
    free_tyVars (TyArr t1 t2) = free_tyVars t1 <> free_tyVars t2
    free_tyVars (TupleT ts) = concatMap free_tyVars ts
    free_tyVars (LazyT t) = free_tyVars t
    free_tyVars (MetaTv _) = []