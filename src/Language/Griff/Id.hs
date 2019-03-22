{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Language.Griff.Id where

import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Language.Griff.Uniq
import Data.Outputable

data Id = Id Text Int
  deriving (Show, Eq, Ord, Generic)

instance Outputable Id

newId :: HasUniq f => Text -> f Id
newId name = Id name <$> newUniq
