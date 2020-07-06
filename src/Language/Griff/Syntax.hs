{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- trees that growをベースに設計
module Language.Griff.Syntax where

import Data.Int (Int32, Int64)
import Data.Kind (Constraint, Type)
import Language.Griff.Id
import Language.Griff.Prelude
import Language.Griff.Pretty
import qualified Text.PrettyPrint.HughesPJ as P
import Text.Megaparsec.Pos (SourcePos)

-- Unboxed literal

data Unboxed = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String String
  deriving stock (Show, Eq, Ord)

instance Pretty Unboxed where
  pPrint (Int32 i) = P.int (fromIntegral i) <> "#"
  pPrint (Int64 i) = P.int (fromIntegral i) <> "L#"
  pPrint (Float f) = P.float f <> "F#"
  pPrint (Double d) = P.double d <> "#"
  pPrint (Char c) = P.quotes (P.char c) <> "#"
  pPrint (String s) = P.doubleQuotes (P.text s) <> "#"

-- Expression

data Exp x
  = Var (XVar x) (XId x) -- variable and constructor
  | Unboxed (XUnboxed x) Unboxed
  | Apply (XApply x) (Exp x) (Exp x)
  | OpApp (XOpApp x) (Exp x) (Exp x) (Exp x)
  | Fn (XFn x) [Clause x]

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x) => Eq (Exp x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x) => Show (Exp x)

instance (Pretty (XId x)) => Pretty (Exp x) where
  pPrintPrec _ _ (Var _ i) = pPrint i
  pPrintPrec _ _ (Unboxed _ u) = pPrint u
  pPrintPrec l d (Apply _ e1 e2) = P.maybeParens (d > 10) $ P.sep [pPrintPrec l 10 e1, pPrintPrec l 11 e2]
  pPrintPrec l d (OpApp _ o e1 e2) = P.maybeParens (d > 10) $ P.sep [pPrintPrec l 10 o, pPrintPrec l 11 e1, pPrintPrec l 11 e2]
  pPrintPrec l _ (Fn _ cs) = P.braces $ foldl1 (\a b -> P.sep [a, "|" <+> b]) $ map (pPrintPrec l 0) cs

-- Clause

data Clause x = Clause (XClause x) [Pat x] (Exp x)

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x) => Show (Clause x)

instance (Pretty (XId x)) => Pretty (Clause x) where
  pPrint (Clause _ pats e) = P.sep (map pPrint pats) <+> "->" <+> pPrint e

-- Pattern

data Pat x
  = VarP (XVarP x) (XId x)
  | ConP (XConP x) (XId x) [Pat x]

deriving stock instance ForallPatX Eq x => Eq (Pat x)

deriving stock instance ForallPatX Show x => Show (Pat x)

instance (Pretty (XId x)) => Pretty (Pat x) where
  pPrintPrec _ _ (VarP _ i) = pPrint i
  pPrintPrec l d (ConP _ i ps) = P.maybeParens (d > 10) $ pPrint i <+> P.sep (map (pPrintPrec l 11) ps)

-- Extension

-- Exp Extensions
type family XVar x

type family XId x

type family XUnboxed x

type family XApply x

type family XOpApp x

type family XFn x

type ForallExpX (c :: Type -> Constraint) x = (c (XVar x), c (XId x), c (XUnboxed x), c (XApply x), c (XOpApp x), c (XFn x))

data GriffPhase = Parse | Rename | TypeCheck

-- Clause Extensions
type family XClause x

type ForallClauseX (c :: Type -> Constraint) x = c (XClause x)

-- Pat Extensions
type family XVarP x

type family XConP x

type ForallPatX (c :: Type -> Constraint) x = (c (XVarP x), c (XConP x), c (XId x))

-- Phase and type instance
data Griff (p :: GriffPhase)

-- data NoExt = NoExt
--   deriving stock (Eq, Show)

-- instance Pretty NoExt where
--   pPrint NoExt = "_"

type instance XVar (Griff _) = SourcePos

type instance XId (Griff p) = GriffId p

type family GriffId (p :: GriffPhase) where
  GriffId 'Parse = Name
  GriffId 'Rename = Id ()

type instance XUnboxed (Griff _) = SourcePos

type instance XApply (Griff _) = SourcePos

type instance XOpApp (Griff _) = SourcePos

type instance XFn (Griff _) = SourcePos

type instance XClause (Griff _) = SourcePos

type instance XVarP (Griff _) = SourcePos

type instance XConP (Griff _) = SourcePos