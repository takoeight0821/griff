{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Griff.ELambda where

import           Data.Outputable
import           GHC.Generics
import           Language.Griff.Id

-- Enriched Lambda Calculus

data Constant = Int Integer
              | Char Char
              | String String
              | Bool Bool
  deriving (Eq, Ord, Show, Generic)

data Exp = Const Constant
         | Var Id
         | Apply Exp Exp
         | Lambda Pat Exp
         | Let Pat Exp Exp
         | LetRec [(Pat, Exp)] Exp
         | Else Exp Exp
         | Case Id [(Pat, Exp)]
         | Error
         | Prim Primitive
         | Match [Id] [([Pat], Exp)] Exp
  deriving (Eq, Ord, Show, Generic)

data Primitive = Add | Sub
  deriving (Eq, Ord, Show, Generic)

data Pat = VarP Id
         | ConstantP Constant
         | ConstructorP Id [Pat]
  deriving (Eq, Ord, Show, Generic)

instance Outputable Constant
instance Outputable Exp
instance Outputable Primitive
instance Outputable Pat
