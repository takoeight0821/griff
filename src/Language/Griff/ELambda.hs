{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Griff.ELambda where

import           Control.Lens            hiding (Const)
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
         | ConstructorP Id [Id]
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Exp
instance Outputable Primitive
instance Outputable Pat

elambdaExample :: Exp
elambdaExample =
  LetRec (VarP sum)
  (Lambda (VarP x)
   (Lambda (VarP acc)
    (Let (VarP b) (Apply (Apply (Prim Eq) (Var x))
                   (Const $ Int 0))
     (Case b
      [ (ConstructorP true [], Var acc)
      , (ConstructorP false [], Apply (Apply (Var sum) (Apply (Apply (Prim Add) (Var x)) (Const $ Int (-1)))) (Apply (Apply (Prim Add) (Var acc)) (Var x)))]))))
  (Apply (Apply (Var sum) (Const $ Int 10)) (Const $ Int 0))
  where
    sum = Id "sum" 0
    x = Id "x" 1
    acc = Id "acc" 2
    true = Id "True" 3
    false = Id "False" 4
    b = Id "b" 5

elambdaEnvExample :: [(Id, (Int, Int))]
elambdaEnvExample =
  [ (Id "True" 3, (0, 0))
  , (Id "False" 4, (1, 0))
  ]
