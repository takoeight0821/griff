{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.TypeRep where

import Language.Griff.Id
import Language.Griff.Prelude
import Language.Griff.Pretty
import qualified Text.PrettyPrint.HughesPJ as P

data Kind = Star | KArr Kind Kind
  deriving stock (Eq, Ord, Show)

instance Pretty Kind where
  pPrintPrec _ _ Star = "*"
  pPrintPrec l d (KArr k1 k2) = P.maybeParens (d > 10) $ pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2

type Sigma = Type
type Rho = Type -- No top-level Forall
type Tau = Type -- No Foralls anywhere

data Type
  = Forall [Id Kind] Rho
  | TyApp Type [Type]
  | TyVar TyVar
  | TyCon (Id Kind)
  | PrimT PrimT
  | TyArr Type Type
  | TupleT [Type]
  | LazyT Type
  | MetaTv TyMeta
  deriving stock (Eq, Show, Ord)

instance Pretty Type where
  pPrintPrec _ d (Forall vs t) = P.maybeParens (d > 9)  $ "forall" <+> P.sep (map pPrint vs) <> "." <+> pPrint t
  pPrintPrec l d (TyApp c ts) = P.maybeParens (d > 11) $ pPrint c <+> P.sep (map (pPrintPrec l 12) ts)
  pPrintPrec _ _ (TyVar v) = pPrint v
  pPrintPrec _ _ (TyCon c) = pPrint c
  pPrintPrec _ _ (PrimT p) = pPrint p
  pPrintPrec l d (TyArr t1 t2) = P.maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TupleT ts) = P.parens $ P.sep $ P.punctuate "," $ map pPrint ts
  pPrintPrec _ _ (LazyT t) = P.braces $ pPrint t
  pPrintPrec _ _ (MetaTv m) = pPrint m

data TyVar
  = BoundTv (Id Kind)
  | SkolemTv (Id Kind)
  deriving stock (Eq, Show, Ord)

instance Pretty TyVar where
  pPrint (BoundTv x) = pPrint x
  pPrint (SkolemTv x) = "_" <> pPrint x

data PrimT = IntT | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord)

instance Pretty PrimT where
  pPrint IntT = "Int#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"

data TyMeta = TyMeta (Id Kind) TyRef

type TyRef = IORef (Maybe Tau)

instance Eq TyMeta where
  (TyMeta u1 _) == (TyMeta u2 _) = u1 == u2

instance Ord TyMeta where
  compare (TyMeta u1 _) (TyMeta u2 _) = compare u1 u2

instance Show TyMeta where
  show (TyMeta u _) = "TyMeta " <> show u

instance Pretty TyMeta where
  pPrint (TyMeta u _) = pPrint u