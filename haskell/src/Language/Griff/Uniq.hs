{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
module Language.Griff.Uniq where

import           Capability.State

type HasUniq m = HasState "uniq" Int m

newUniq :: HasUniq m => m Int
newUniq = do
  i <- get @"uniq"
  modify @"uniq" (+1)
  return i
