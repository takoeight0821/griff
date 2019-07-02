{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyDataDeriving  #-}
{-# LANGUAGE KindSignatures     #-}
module Language.Griff.Syntax where

import           Control.Lens.Plated
import           Data.Data
import           Data.Outputable
import           Data.Text           (Text)
import           GHC.Generics
import           Text.Megaparsec.Pos

instance Outputable Pos where
  pprPrec i x = pprPrec i $ unPos x

instance Outputable SourcePos

data Exp a = Var SourcePos a
           | Int SourcePos Integer
           | Char SourcePos Char
           | String SourcePos Text
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

instance Outputable a => Outputable (Exp a)
instance Data a => Plated (Exp a)

data Op = Add | Sub | Mul | Div | Mod | FAdd | FSub | FMul | FDiv | Eq | Neq | Lt | Le | Gt | Ge | And | Or
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Op

data Pat a = VarP SourcePos a
           | IntP SourcePos Integer
           | CharP SourcePos Char
           | StringP SourcePos Text
           | ConstructorP SourcePos a [Pat a]
  deriving (Eq, Ord, Show, Generic, Data, Foldable)

instance Outputable a => Outputable (Pat a)

data Dec a = ScSig SourcePos a (Type a)
           | ScDef SourcePos a [a] (Exp a)
           | TypeAliasDef SourcePos a [a] (Type a)
  deriving (Eq, Ord, Show, Generic, Data)

type ConstructorDef a = (a, [Type a])

instance Outputable a => Outputable (Dec a)

data Type a = TyApp SourcePos (Type a) (Type a)
            | TyVar SourcePos a
            | TyArr SourcePos (Type a) (Type a)
            | TyCon SourcePos a
            | TyRecord SourcePos [(Text, Type a)]
            | TyVariant SourcePos [(Text, Type a)]
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable a => Outputable (Type a)
