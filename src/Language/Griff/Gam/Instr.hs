module Language.Griff.Gam.Instr where

import Language.Griff.Id

data Sc = Sc Id Code
  deriving (Show, Eq, Ord)

type Code = [Instr]

type Tag = Int
type Arity = Int

data Instr = Ldi Integer
           | Ldsc Id
           | Access Int
           | Closure Code
           | Let
           | EndLet
           | Test Code Code
           | Prim Prim
           | Block Tag Arity
           | Field Int
           | Invoke Tag Code
           | Apply
           | TailApply
           | PushMark
           | Grab
           | Return
  deriving (Show, Eq, Ord)

data Prim = Add | Eq
  deriving (Show, Eq, Ord)
