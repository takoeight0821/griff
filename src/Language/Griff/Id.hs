{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Griff.Id where

import Control.Effect
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

newId :: (Carrier sig f, Member (State Uniq) sig) => Text -> f Id
newId name = Id . (name <>) . pack . show <$> newUniq
