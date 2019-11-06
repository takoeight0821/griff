{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Griff.Prelude
  ( module Prelude
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Data.List.NonEmpty
  , Text
  , Set
  , firstM
  , secondM
  ) where

import           Data.Bifunctor
import           Data.Bitraversable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map
import           Data.Set           (Set)
import           Data.Text          (Text)
import           Prelude            hiding (lookup)
import           Text.Show.Pretty

firstM :: (Applicative f, Bitraversable t) => (a -> f c) -> t a b -> f (t c b)
firstM f = bitraverse f pure

secondM :: (Applicative f, Bitraversable t) => (b -> f d) -> t a b -> f (t a d)
secondM = bitraverse pure

instance (PrettyVal a, PrettyVal b) => PrettyVal (Map a b) where
  prettyVal x = Con "fromList" [prettyVal $ assocs x]

instance PrettyVal a => PrettyVal (NonEmpty a)
