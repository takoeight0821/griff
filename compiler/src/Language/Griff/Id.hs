{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Griff.Id where

import           Data.Data
import           Data.Outputable
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Language.Griff.Uniq

data Id = Id Text Int
  deriving (Show, Eq, Ord, Generic, Data)

instance Outputable Id

newId :: HasUniq f => Text -> f Id
newId name = Id name <$> newUniq
