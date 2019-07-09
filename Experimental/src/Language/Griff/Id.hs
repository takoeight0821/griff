{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Griff.Id where

import           Data.Data
import           Data.Outputable
import           Data.String
import           Data.Text           (Text, unpack, pack)
import           GHC.Generics        (Generic)
import           Language.Griff.Uniq

newtype Id = Id Text
  deriving (Show, Eq, Ord, Generic, Data)

instance Outputable Id

stringify :: IsString a => Id -> a
stringify (Id text) = fromString $ unpack text

newId :: HasUniq f => Text -> f Id
newId name = Id . (name <>) . pack . show <$> newUniq
