{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Griff.Lambda where

import           Control.Lens             hiding (Const)
import           Data.Data
import           Data.Outputable
import           GHC.Generics
import           Language.Griff.ConsTable
import           Language.Griff.Constant
import           Language.Griff.Id

-- Lambda Calculus

-- -- sum typeをtagで区別する
-- type Tag = Int
-- -- constructorのarity
-- type Arity = Int

defaultTag :: Tag
defaultTag = -1 -- tag for default clause

data Exp = Const Constant
         | Var Id
         | Apply Exp Exp
         | Lambda Id Exp
         | Let Id Exp Exp
         | LetRec Id Exp Exp
         | Op Op Exp Exp
         | Select Int Exp
         | Pack Tag [Exp]
         | If Exp Exp Exp -- constantに対するcaseはifに変換
         | Switch Id [(Tag, Exp)] -- sum typeのconstructorに対するcaseはswitchに変換
  deriving (Eq, Ord, Show, Generic, Data)

instance Plated Exp

data Op = Add | Sub | Mul | Div | Mod -- Int
        | FAdd | FSub | FMul | FDiv -- Float
        | Eq | Neq -- Eq
        | Lt | Le | Gt | Ge -- Ord
        | And | Or -- Bool
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Exp
instance Outputable Op

{- コンストラクタの表現

{Tag Arity}

Nil -> {0 0}
Cons -> {1 2}

Pair -> {0 2}
-}


example1 :: Exp
example1 =
  LetRec sum (Lambda x (Lambda acc
                        (If (Op Eq (Var x) (Const $ Int 0))
                          (Var acc)
                          (Apply
                           (Apply (Var sum) (Op Add (Var x) (Const $ Int (-1))))
                            (Op Add (Var acc) (Var x))))))
  (Apply (Apply (Var sum) (Const $ Int 10)) (Const $ Int 0))
  where
    sum = Id "sum" 0
    x = Id "x" 1
    acc = Id "acc" 2
