{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Language.Griff.ELambda where

import           Control.Lens
import           Data.Data
import           Data.Outputable
import           GHC.Generics
import           Language.Griff.Constant
import           Language.Griff.Id

-- Enriched Lambda Calculus

data Exp = Const Constant
         | Var Id
         | Apply Exp Exp
         | Lambda Pat Exp
         | Let Pat Exp Exp
         | LetRec Pat Exp Exp
         | Else Exp Exp
         | Case Id Pat Exp Exp
         | Error
         | Prim Primitive
         | Match [Id] [Pat] Exp Exp
  deriving (Eq, Ord, Show, Generic, Data)

instance Plated Exp

data Primitive = Add | Sub
  deriving (Eq, Ord, Show, Generic, Data)

data Pat = VarP Id
         | ConstantP Constant
         | ConstructorP Id [Pat]
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Exp
instance Outputable Primitive
instance Outputable Pat
