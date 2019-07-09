{-# LANGUAGE DeriveGeneric #-}
module Language.Griff.IR.LIR where

import           Data.Outputable
import           GHC.Generics

data Exp = Int Integer
  deriving (Eq, Ord, Show, Generic)

instance Outputable Exp
