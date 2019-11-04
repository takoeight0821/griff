{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Griff.Prelude
  ( module Prelude
  , module Data.Bifunctor
  , module Data.Bitraversable
  , firstM
  , secondM
  ) where

import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Map
import           Prelude            hiding (lookup)
import           Text.Show.Pretty

firstM :: (Applicative f, Bitraversable t) => (a -> f c) -> t a b -> f (t c b)
firstM f = bitraverse f pure

secondM :: (Applicative f, Bitraversable t) => (b -> f d) -> t a b -> f (t a d)
secondM = bitraverse pure

instance (PrettyVal a, PrettyVal b) => PrettyVal (Map a b) where
  prettyVal x = Con "fromList" [prettyVal $ assocs x]
