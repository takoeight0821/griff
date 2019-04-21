{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Griff.Instr where

import Data.Data
import GHC.Generics

type Code = [Instr]

type Tag = Int

data Instr = Ldi Integer
           | Access Int
           | Closure Code
           | Let
           | EndLet
           | Test Code Code
           | Add
           | Eq
           | Block Tag Int
           | Field Int
           | Invoke Tag Code
           | Apply
           | TailApply
           | PushMark
           | Grab
           | Return
  deriving (Show, Eq, Ord, Generic, Data)
