{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Language.Griff.Core where

import           Data.Map
import           Data.Outputable
import           Data.Text
import           GHC.Generics
import           Language.Griff.Id
import           Language.Griff.TypeRep

type Env = Map Id Scheme

data Exp = Var Id
         | Int Integer
         | Char Char
         | String Text
         | Bool Bool
         | Record [(Text, Exp)]
         | Apply Exp Exp
         | Lambda Id Exp
         | Let Id Exp Exp
         | LetRec [(Id, Exp)] Exp
         | Prim Op [Exp]
         | Case Exp [(Pat, Exp)]
  deriving (Eq, Show, Generic, Outputable)

data Op = Add | Sub | Mul | Div | Mod | Eq | Neq
        | Lt | Le | Gt | Ge | And | Or
        | Error
  deriving (Eq, Show, Generic, Outputable)

data Pat = VarP Id
         | BoolP Bool
         | RecordP [(Text, Pat)]
         | VariantP Text Pat
  deriving (Eq, Show, Generic, Outputable)

data Dec = ScDef Id Exp
  deriving (Eq, Show, Generic, Outputable)
