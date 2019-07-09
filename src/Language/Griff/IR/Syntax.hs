{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Language.Griff.IR.Syntax where

import           Data.Data
import           Data.Outputable
import           GHC.Generics
import           Text.Megaparsec.Pos

instance Outputable Pos where
  pprPrec i x = pprPrec i $ unPos x

instance Outputable SourcePos

data Exp a = Int SourcePos Integer
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable a => Outputable (Exp a)
