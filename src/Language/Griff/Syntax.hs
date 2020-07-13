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
import Data.Kind (Constraint)
import qualified Data.Kind as K
import Language.Griff.Id
import Language.Griff.Prelude
import Language.Griff.Pretty
import qualified Language.Griff.TypeRep as T
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
  = Var (XVar x) (XId x) -- variable
  | Con (XCon x) (XId x) -- constructor
  | Unboxed (XUnboxed x) Unboxed
  | Apply (XApply x) (Exp x) (Exp x)
  | OpApp (XOpApp x) (XId x) (Exp x) (Exp x)
  | Fn (XFn x) [Clause x]
  | Tuple (XTuple x) [Exp x]
  | Force (XForce x) (Exp x)

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x) => Eq (Exp x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x) => Show (Exp x)

instance (Pretty (XId x)) => Pretty (Exp x) where
  pPrintPrec _ _ (Var _ i) = pPrint i
  pPrintPrec _ _ (Con _ c) = pPrint c
  pPrintPrec _ _ (Unboxed _ u) = pPrint u
  pPrintPrec l d (Apply _ e1 e2) = P.maybeParens (d > 10) $ P.sep [pPrintPrec l 10 e1, pPrintPrec l 11 e2]
  pPrintPrec l d (OpApp _ o e1 e2) =
    P.maybeParens (d > 10) $
      P.sep [pPrintPrec l 11 e1, pPrintPrec l 10 o, pPrintPrec l 11 e2]
  pPrintPrec l _ (Fn _ cs) =
    P.braces $
      P.space
        <> foldl1
          (\a b -> P.sep [a, P.nest (-2) $ "|" <+> b])
          (map (pPrintPrec l 0) cs)
  pPrintPrec _ _ (Tuple _ xs) = P.parens $ P.sep $ P.punctuate "," $ map pPrint xs
  pPrintPrec l _ (Force _ x) = pPrintPrec l 11 x <> "!"

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
  pPrintPrec _ _ (ConP _ i []) = pPrint i
  pPrintPrec l d (ConP _ i ps) = P.maybeParens (d > 10) $ pPrint i <+> P.sep (map (pPrintPrec l 11) ps)

-- Type

data Type x
  = TyApp (XTyApp x) (Type x) [Type x]
  | TyVar (XTyVar x) (XTId x)
  | TyCon (XTyCon x) (XTId x)
  | TyArr (XTyArr x) (Type x) (Type x)
  | TyTuple (XTyTuple x) [Type x]
  | TyLazy (XTyLazy x) (Type x)

deriving stock instance ForallTypeX Eq x => Eq (Type x)

deriving stock instance ForallTypeX Show x => Show (Type x)

instance (Pretty (XTId x)) => Pretty (Type x) where
  pPrintPrec l d (TyApp _ t ts) = P.maybeParens (d > 11) $ pPrint t <+> P.sep (map (pPrintPrec l 12) ts)
  pPrintPrec _ _ (TyVar _ i) = pPrint i
  pPrintPrec _ _ (TyCon _ i) = pPrint i
  pPrintPrec l d (TyArr _ t1 t2) = P.maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple _ ts) = P.parens $ P.sep $ P.punctuate "," $ map pPrint ts
  pPrintPrec _ _ (TyLazy _ t) = P.braces $ pPrint t

-- Declaration

data Decl x
  = ScDef (XScDef x) (XId x) [XId x] (Exp x)
  | ScSig (XScSig x) (XId x) (Type x)
  | DataDef (XDataDef x) (XTId x) [XTId x] [(XId x, [Type x])]
  | Infix (XInfix x) Assoc Int (XId x)
  | Forign (XForign x) (XId x) (Type x)

deriving stock instance ForallDeclX Eq x => Eq (Decl x)

deriving stock instance ForallDeclX Show x => Show (Decl x)

instance (Pretty (XId x), Pretty (XTId x)) => Pretty (Decl x) where
  pPrint (ScDef _ f xs e) = P.sep [pPrint f <+> P.sep (map pPrint xs) <+> "=", P.nest 2 $ pPrint e]
  pPrint (ScSig _ f t) = pPrint f <+> "::" <+> pPrint t
  pPrint (DataDef _ d xs cs) = P.sep ["data" <+> pPrint d <+> P.sep (map pPrint xs) <+> "=", foldl1 (\a b -> P.sep [a, "|" <+> b]) $ map pprConDef cs]
    where
      pprConDef (con, ts) = pPrint con <+> P.sep (map pPrint ts)
  pPrint (Infix _ a o x) = "infix" <> pPrint a <+> pPrint o <+> pPrint x
  pPrint (Forign _ x t) = "forign import" <+> pPrint x <+> "::" <+> pPrint t

data Assoc = LeftA | RightA | NeutralA
  deriving stock (Eq, Show)

instance Pretty Assoc where
  pPrint LeftA = "l"
  pPrint RightA = "r"
  pPrint NeutralA = ""

-- Extension

-- Exp Extensions
type family XVar x

type family XCon x

type family XId x

type family XUnboxed x

type family XApply x

type family XOpApp x

type family XFn x

type family XTuple x

type family XForce x

type ForallExpX (c :: K.Type -> Constraint) x = (c (XVar x), c (XCon x), c (XId x), c (XUnboxed x), c (XApply x), c (XOpApp x), c (XFn x), c (XTuple x), c (XForce x))

-- Clause Extensions
type family XClause x

type ForallClauseX (c :: K.Type -> Constraint) x = c (XClause x)

-- Pat Extensions
type family XVarP x

type family XConP x

type ForallPatX (c :: K.Type -> Constraint) x = (c (XVarP x), c (XConP x), c (XId x))

-- Type Extensions
type family XTId x

type family XTyApp x

type family XTyVar x

type family XTyCon x

type family XTyArr x

type family XTyTuple x

type family XTyLazy x

type ForallTypeX (c :: K.Type -> Constraint) x = (c (XTyApp x), c (XTyVar x), c (XTyCon x), c (XTyArr x), c (XTyTuple x), c (XTyLazy x), c (XId x), c (XTId x))

-- Decl Extensions

type family XScDef x

type family XScSig x

type family XDataDef x

type family XInfix x

type family XForign x

type ForallDeclX (c :: K.Type -> Constraint) x = (c (XScDef x), c (XScSig x), c (XDataDef x), c (XInfix x), c (XForign x), ForallExpX c x, ForallClauseX c x, ForallPatX c x, ForallTypeX c x)

-- Phase and type instance
data GriffPhase = Parse | Rename | TypeCheck

data Griff (p :: GriffPhase)

type family GriffId (p :: GriffPhase) where
  GriffId 'Parse = Name
  GriffId 'Rename = Id NoMeta
  GriffId 'TypeCheck = Id T.Sigma

type family GriffTId (p :: GriffPhase) where
  GriffTId 'Parse = Name
  GriffTId 'Rename = Id NoMeta
  GriffTId 'TypeCheck = Id NoMeta

type instance XVar (Griff _) = SourcePos

type instance XCon (Griff _) = SourcePos

type instance XId (Griff p) = GriffId p

type instance XUnboxed (Griff _) = SourcePos

type instance XApply (Griff _) = SourcePos

type instance XOpApp (Griff 'Parse) = SourcePos

type instance XOpApp (Griff 'Rename) = (SourcePos, (Assoc, Int))

type instance XFn (Griff _) = SourcePos

type instance XTuple (Griff _) = SourcePos

type instance XForce (Griff _) = SourcePos

type instance XClause (Griff _) = SourcePos

type instance XVarP (Griff _) = SourcePos

type instance XConP (Griff _) = SourcePos

type instance XTId (Griff p) = GriffTId p

type instance XTyApp (Griff _) = SourcePos

type instance XTyVar (Griff _) = SourcePos

type instance XTyCon (Griff _) = SourcePos

type instance XTyArr (Griff _) = SourcePos

type instance XTyTuple (Griff _) = SourcePos

type instance XTyLazy (Griff _) = SourcePos

type instance XScDef (Griff _) = SourcePos

type instance XScSig (Griff _) = SourcePos

type instance XDataDef (Griff _) = SourcePos

type instance XInfix (Griff _) = SourcePos

type instance XForign (Griff 'Parse) = SourcePos

type instance XForign (Griff 'Rename) = (SourcePos, Text)