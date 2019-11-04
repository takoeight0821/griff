{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Griff.Id where

import           Control.Effect
import           Data.Data
import           Data.String
import           Data.Text              (Text, pack, unpack)
import           GHC.Generics           (Generic)
import           Language.Griff.Prelude
import           Language.Griff.Uniq
import           Text.Show.Pretty

newtype Id = Id Text
  deriving (Show, Eq, Ord, Generic, Data)

instance PrettyVal Id

stringify :: IsString a => Id -> a
stringify (Id text) = fromString $ unpack text

newId :: (Carrier sig f, Member Fresh sig) => Text -> f Id
newId name = Id . (name <>) . pack . show <$> newUniq
