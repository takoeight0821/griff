{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Griff.CPS where

import           Data.Map          (Map)
import           Data.Outputable
import           Data.Text         (Text)
import           GHC.Generics
import           Language.Griff.Id

type LVar = (Id, CType)

type AccessPath = (CType, Text) -- record type and field name

data Value = Var LVar
           | Bool Bool
           | Int Int
           | Char Char
           | String String
           | Void
  deriving (Show, Eq, Generic, Outputable)

data POp = Box
         | UnBox CType
  deriving (Show, Eq, Generic, Outputable)

data CExp = Record CType [(Text, Value)] LVar CExp
          | Select Value AccessPath LVar CExp
          | App Value [Value]
          | Fix [Function] CExp
          | Switch Value [(Value, CExp)]
          | Prim POp [Value] LVar CExp
  deriving (Show, Eq, Generic, Outputable)

data CType = CTBox
           | CTArr CType CType
           | CTInt
           | CTChar
           | CTString
           | CTBool
           | CTRecord (Map Text CType)
           | CTVoid
  deriving (Show, Eq, Generic, Outputable)

type Function = (LVar, [LVar], CExp)
