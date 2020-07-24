{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.TcEnv where

import qualified Data.Map as Map
import Language.Griff.MonadUniq
import Language.Griff.Prelude
import Language.Griff.RnEnv (RnEnv)
import qualified Language.Griff.RnEnv as R
import Language.Griff.TypeRep
import Language.Griff.Syntax (Griff, GriffPhase (Rename), XId)

type RnId = XId (Griff 'Rename)

newtype TcEnv = TcEnv
  { _varEnv :: Map RnId Scheme
  }
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''TcEnv

genTcEnv :: MonadUniq m => RnEnv -> m TcEnv
genTcEnv rnEnv = do
  -- lookup RnId of primitive functions and operetors
  let add_i32 = fromJust $ Map.lookup "add_i32#" (view R.varEnv rnEnv)
  let add_i64 = fromJust $ Map.lookup "add_i64#" (view R.varEnv rnEnv)

  pure $
    TcEnv
      { _varEnv =
          Map.fromList
            [ (add_i32, Forall [] (TyTuple [TyPrim Int32T, TyPrim Int32T] `TyArr` TyPrim Int32T)),
              (add_i64, Forall [] (TyTuple [TyPrim Int64T, TyPrim Int64T] `TyArr` TyPrim Int64T))
            ]
      }