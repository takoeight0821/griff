{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
module Language.Griff.Id (Id(..), stringify, newId) where

import           Data.Data
import           Data.String
import           Data.Text           (Text, pack, unpack)
import           GHC.Generics
import           Language.Griff.Uniq

newtype Id = Id Text
  deriving (Show, Eq, Ord, Generic, Data)

stringify :: IsString a => Id -> a
stringify (Id text) = fromString $ unpack text

newId :: HasUniq m => Text -> m Id
newId name = Id . (name<>) . pack . show <$> newUniq
