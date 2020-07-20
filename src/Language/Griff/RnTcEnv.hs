{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.RnTcEnv where

import qualified Data.Map as Map
import Language.Griff.Id
import Language.Griff.MonadUniq
import Language.Griff.Prelude
import Language.Griff.Syntax (Assoc)

type RnId = Id NoMeta

newtype RnState = RnState {_infixInfo :: Map RnId (Assoc, Int)}
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''RnState

data RnEnv = RnEnv
  { _varEnv :: Map Name RnId,
    _typeEnv :: Map Name RnId
  }
  deriving stock (Show)

instance Semigroup RnEnv where
  RnEnv v1 t1 <> RnEnv v2 t2 = RnEnv (v1 <> v2) (t1 <> t2)

instance Monoid RnEnv where
  mempty = RnEnv mempty mempty

makeLenses ''RnEnv

genRnState :: Monad m => m RnState
genRnState = pure $ RnState mempty

genRnEnv :: MonadUniq m => m RnEnv
genRnEnv = do
  -- generate RnId of primitive functions and operetors
  add_i32 <- newId NoMeta "add_i32#"
  add_i64 <- newId NoMeta "add_i64#"
  -- generate RnId of primitive types
  bool_t <- newId NoMeta "Bool#"
  int32_t <- newId NoMeta "Int32#"
  int64_t <- newId NoMeta "Int64#"
  float_t <- newId NoMeta "Float#"
  double_t <- newId NoMeta "Double#"
  char_t <- newId NoMeta "Char#"
  string_t <- newId NoMeta "String#"
  pure $
    RnEnv
      { _varEnv =
          Map.fromList
            [ ("add_i32#", add_i32),
              ("add_i64#", add_i64)
            ],
        _typeEnv =
          Map.fromList
            [ ("Bool#", bool_t),
              ("Int32#", int32_t),
              ("Int64#", int64_t),
              ("Float#", float_t),
              ("Double#", double_t),
              ("Char#", char_t),
              ("String#", string_t)
            ]
      }