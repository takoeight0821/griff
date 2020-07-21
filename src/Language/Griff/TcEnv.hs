{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.TcEnv where

import qualified Data.Map as Map
import Language.Griff.Id
import Language.Griff.MonadUniq
import Language.Griff.Prelude
import Language.Griff.RnEnv (RnEnv)
import qualified Language.Griff.RnEnv as R
import Language.Griff.TypeRep
import Language.Griff.Syntax (Griff, GriffPhase (Rename, TypeCheck), XId)

type RnId = XId (Griff 'Rename)

type TcId = XId (Griff 'TypeCheck)

newtype TcEnv = TcEnv
  { _varEnv :: Map RnId TcId
  }
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''TcEnv

genTcEnv :: MonadUniq m => RnEnv -> m TcEnv
genTcEnv rnEnv = do
  -- generate TcId of primitive functions and operetors
  add_i32 <- newId (Forall [] (TupleT [PrimT Int32T, PrimT Int32T] `TyArr` PrimT Int32T)) "add_i32#"
  let add_i32_rn = fromJust $ Map.lookup "add_i32#" (view R.varEnv rnEnv)

  add_i64 <- newId (Forall [] (TupleT [PrimT Int64T, PrimT Int64T] `TyArr` PrimT Int64T)) "add_i64#"
  let add_i64_rn = fromJust $ Map.lookup "add_i64#" (view R.varEnv rnEnv)

  pure $
    TcEnv
      { _varEnv =
          Map.fromList
            [ (add_i32_rn, add_i32),
              (add_i64_rn, add_i64)
            ]
      }