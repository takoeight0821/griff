{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Griff.Id (Id(..), stringify) where

import           Data.Data
import           Data.String
import           Data.Text    (Text, unpack)
import           GHC.Generics

newtype Id = Id Text
  deriving (Show, Eq, Ord, Generic, Data)

stringify :: IsString a => Id -> a
stringify (Id text) = fromString $ unpack text
