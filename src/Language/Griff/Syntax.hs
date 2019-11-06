{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyDataDeriving  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Language.Griff.Syntax where

import           Control.Lens
import           Data.Data
import           GHC.Generics
import           Language.Griff.Prelude
import           Language.Griff.TypeRep (TPrim (..))
import           Text.Megaparsec.Pos
import           Text.Show.Pretty

instance PrettyVal Pos where
  prettyVal = prettyVal . unPos

instance PrettyVal SourcePos

data Exp a = Var SourcePos a
           | Int SourcePos Integer
           | Char SourcePos Char
           | String SourcePos Text
           | Bool SourcePos Bool
           | Record SourcePos [(Text, Exp a)]
           | Proj SourcePos Text (Exp a)
           | Ascribe SourcePos (Exp a) (Type a)
           | Apply SourcePos (Exp a) (Exp a)
           | Lambda SourcePos a (Exp a)
           | Let SourcePos a [a] (Exp a) (Exp a)
           | LetRec SourcePos [(a, [a], Exp a)] (Exp a)
           | BinOp SourcePos Op (Exp a) (Exp a)
           | Case SourcePos (Exp a) [(Pat a, Exp a)]
           | If SourcePos (Exp a) (Exp a) (Exp a)
  deriving (Eq, Ord, Show, Generic, Data)

instance PrettyVal a => PrettyVal (Exp a)
instance Data a => Plated (Exp a)

data Op = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Le | Gt | Ge | And | Or
  deriving (Eq, Ord, Show, Generic, Data)

instance PrettyVal Op

data Pat a = VarP SourcePos a
           | RecordP SourcePos [(Text, Pat a)]
           | VariantP SourcePos Text (Pat a) (Type a)
  deriving (Eq, Ord, Show, Generic, Data)

instance PrettyVal a => PrettyVal (Pat a)

data Dec a = ScSig SourcePos a (Type a)
           | ScDef SourcePos a [a] (Exp a)
           | TypeAliasDef SourcePos a [a] (Type a)
  deriving (Eq, Ord, Show, Generic, Data)

type ConstructorDef a = (a, [Type a])

instance PrettyVal a => PrettyVal (Dec a)

data Type a = TyApp SourcePos a [Type a]
            | TyVar SourcePos a
            | TyPrim SourcePos TPrim
            | TyArr SourcePos (Type a) (Type a)
            | TyRecord SourcePos [(Text, Type a)]
            | TyVariant SourcePos [(Text, Type a)]
  deriving (Eq, Ord, Show, Generic, Data)

instance PrettyVal a => PrettyVal (Type a)

makePrisms ''Dec
