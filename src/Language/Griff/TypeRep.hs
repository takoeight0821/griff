{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.TypeRep where

import Data.IORef (IORef)
import Language.Griff.Id
import Language.Griff.Prelude
import Language.Griff.Pretty
import qualified Text.PrettyPrint.HughesPJ as P

data Scheme = Forall [Id NoMeta] Type
  deriving stock (Eq, Show, Ord)

data Type
  = TyApp Type [Type]
  | TyVar TyVar
  | TyCon (Id NoMeta)
  | TyArr Type Type
  | TupleT [Type]
  | LazyT Type
  | PrimT PrimT
  | MetaTv TyMeta
  deriving stock (Eq, Show, Ord)

instance Pretty Type where
  pPrintPrec l d (TyApp c ts) = P.maybeParens (d > 11) $ pPrint c <+> P.sep (map (pPrintPrec l 12) ts)
  pPrintPrec _ _ (TyVar v) = pPrint v
  pPrintPrec _ _ (TyCon c) = pPrint c
  pPrintPrec l d (TyArr t1 t2) = P.maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TupleT ts) = P.parens $ P.sep $ P.punctuate "," $ map pPrint ts
  pPrintPrec _ _ (LazyT t) = P.braces $ pPrint t
  pPrintPrec _ _ (PrimT p) = pPrint p
  pPrintPrec _ _ (MetaTv m) = pPrint m

data TyVar
  = BoundTv (Id NoMeta)
  | SkolemTv (Id NoMeta)
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

data TyMeta = TyMeta (Id NoMeta) (IORef (Maybe Type))

instance Eq TyMeta where
  (TyMeta u1 _) == (TyMeta u2 _) = u1 == u2

instance Ord TyMeta where
  compare (TyMeta u1 _) (TyMeta u2 _) = compare u1 u2

instance Show TyMeta where
  show (TyMeta u _) = "TyMeta " <> show u

instance Pretty TyMeta where
  pPrint (TyMeta u _) = pPrint u