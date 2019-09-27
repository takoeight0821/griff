{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts          #-}
module Language.Griff.Uniq (Uniq, newUniq, runUniq) where

import Control.Effect.State

newtype Uniq = Uniq Int
  deriving (Eq, Show)

newUniq :: (Carrier sig m, Member (State Uniq) sig) => m Int
newUniq = do
  Uniq i <- get
  modify (\_ -> Uniq (i + 1))
  return i

runUniq :: Functor m => StateC Uniq m a -> m a
runUniq = evalState (Uniq 0)
