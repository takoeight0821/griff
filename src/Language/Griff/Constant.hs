{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Language.Griff.Constant where

import           Data.Data
import           Data.Outputable
import           GHC.Generics

data Constant = Int Integer
              | Char Char
              | String String
              | Bool Bool
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Constant
