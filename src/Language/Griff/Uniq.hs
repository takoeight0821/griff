{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Griff.Uniq (newUniq, runUniq) where

import           Control.Carrier.Fresh.Strict
import           Language.Griff.Prelude

newUniq :: (Has Fresh sig m) => m Int
newUniq = fresh

runUniq :: Functor m => FreshC m a -> m a
runUniq = evalFresh 0
