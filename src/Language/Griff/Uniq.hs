{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Griff.Uniq (newUniq, runUniq) where

import           Control.Effect.Fresh
import           Language.Griff.Prelude

newUniq :: (Member Fresh sig, Carrier sig m) => m Int
newUniq = fresh

runUniq :: Functor m => FreshC m a -> m a
runUniq = runFresh
