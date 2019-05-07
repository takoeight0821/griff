{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module Language.Griff.ConsTable where

import Capability.State
import Data.Map as Map
import Language.Griff.Id

type Tag = Int
type Arity = Int
type ConsTable = Map Id (Tag, Arity)

type HasConsTable m = HasState "consTable" ConsTable m

lookupCons :: HasConsTable m => Id -> m (Tag, Arity)
lookupCons c = do
  table <- get @"consTable"
  case Map.lookup c table of
    Nothing -> error "lookupCons"
    Just x -> return x

defaultConsTable :: ConsTable
defaultConsTable = Map.empty
