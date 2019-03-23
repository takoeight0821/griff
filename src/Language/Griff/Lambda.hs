{-# LANGUAGE DeriveGeneric #-}
module Language.Griff.Lambda where

import           Data.Outputable
import           GHC.Generics
import           Language.Griff.Id

-- Lambda Calculus

data Constant = Int Integer
              | Char Char
              | String String
              | Bool Bool
  deriving (Eq, Ord, Show, Generic)

-- sum typeをtagで区別する
type Tag = Int
-- constructorのarity
type Arity = Int

data Exp = Const Constant
         | Var Id
         | Apply Exp Exp
         | Lambda Id Exp
         | Let Id Exp Exp
         | LetRec [(Id, Exp)] Exp
         | Prim Primitive
         | If Exp Exp Exp -- constantに対するcaseはifに変換
         | Switch Exp [(Tag, Exp)] -- sum typeのconstructorに対するcaseはswitchに変換
         | Constructor Tag Arity
  deriving (Eq, Ord, Show, Generic)

data Primitive = Add | Sub
               | Sel Arity Int
  deriving (Eq, Ord, Show, Generic)

instance Outputable Constant
instance Outputable Exp
instance Outputable Primitive

{- コンストラクタの表現

{Tag Arity}

Nil -> {1 0}
Cons -> {2 2}

Pair -> {1 2}
-}
