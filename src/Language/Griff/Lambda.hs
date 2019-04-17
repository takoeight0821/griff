{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Language.Griff.Lambda where

import           Control.Lens
import           Data.Data
import           Data.Outputable
import           GHC.Generics
import           Language.Griff.Constant
import           Language.Griff.Id

-- Lambda Calculus

-- sum typeをtagで区別する
type Tag = Int
-- constructorのarity
type Arity = Int

data Exp = Const Constant
         | Var Id
         | Apply Exp Exp
         | Lambda Id Exp
         | Let Id Exp Exp
         | LetRec Id Exp Exp
         | Prim Primitive
         | If Exp Exp Exp -- constantに対するcaseはifに変換
         | Switch Id [(Tag, Exp)] -- sum typeのconstructorに対するcaseはswitchに変換
  deriving (Eq, Ord, Show, Generic, Data)

instance Plated Exp

data Primitive = Add | Sub | Mul | Div | Mod -- Int
               | FAdd | FSub | FMul | FDiv -- Float
               | Eq | Neq -- Eq
               | Lt | Le | Gt | Ge -- Ord
               | And | Or -- Bool
               | Sel Arity Int -- unwrap Constructor
               | Pack Tag Arity -- wrap Constructor
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Exp
instance Outputable Primitive

{- コンストラクタの表現

{Tag Arity}

Nil -> {1 0}
Cons -> {2 2}

Pair -> {1 2}
-}
