{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts          #-}
module Language.Griff.Uniq (newUniq, runUniq) where

import Control.Effect.Fresh

newUniq :: (Member Fresh sig, Carrier sig m) => m Int
newUniq = fresh

runUniq :: Functor m => FreshC m a -> m a
runUniq = runFresh 
