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
-- 『The Implementation of Functional Programming Language』

data Exp = Const Constant
         | Var Id
         | Constructor Id
         | Apply Exp Exp
         | Lambda Pat Exp -- 引数をPatで分解する。Caseを使った式に変換される
         | Let Pat Exp Exp
         | LetRec Pat Exp Exp
         | Case Id [(Pat, Exp)]
         | Prim Primitive
  deriving (Eq, Ord, Show, Generic, Data)

instance Plated Exp

data Primitive = Add | Sub | Mul | Div | Mod -- Int
               | FAdd | FSub | FMul | FDiv -- Float
               | Eq | Neq -- Eq
               | Lt | Le | Gt | Ge -- Ord
               | And | Or -- Bool
  deriving (Eq, Ord, Show, Generic, Data)

data Pat = VarP Id
         | ConstantP Constant
         | ConstructorP Id [Pat]
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Exp
instance Outputable Primitive
instance Outputable Pat
