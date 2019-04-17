{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Griff.Instr where

import Data.Data
import GHC.Generics

type Code = [Instr]

data Instr = Ldi Integer
           | Ldb Bool
           | Access Int
           | Closure Code
           | Apply
           | Return
           | Let
           | EndLet
           | Test Code Code
           | Add
           | Eq
  deriving (Show, Eq, Ord, Generic, Data)