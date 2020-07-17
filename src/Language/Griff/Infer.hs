{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Infer where

import Language.Griff.Prelude
import Data.List ((\\))
import Language.Griff.Id
import Language.Griff.MonadUniq
import Language.Griff.Syntax
import Language.Griff.Pretty
import Language.Griff.TcMonad
import Language.Griff.TypeRep
import Text.Megaparsec.Pos (SourcePos)

tcExp e = do
  ty <- inferSigma e
  zonkType ty

-----------------------
-- The expected type --
-----------------------

data Expected a = Infer (IORef a) | Check a

-----------------------------
-- tcRho, and its variants --
-----------------------------

checkRho expr ty = tcRho expr (Check ty)

inferRho expr = do
  ref <- newIORef (error "inferRho: empty result")
  tcRho expr (Infer ref)
  readIORef ref

tcRho :: (MonadIO m, MonadUniq m, MonadReader TcEnv m) => Exp (Griff 'Rename) -> Expected Tau -> m (Exp (Griff 'TypeCheck))
tcRho (Var pos v) exp_ty = do
  v_sigma <- lookupVar pos v
  ty <- instSigma pos v_sigma exp_ty
  pure $ Var pos (v & idMeta .~ ty)
tcRho (Con pos c) exp_ty = do
  c_sigma <- lookupVar pos c
  ty <- instSigma pos c_sigma exp_ty
  pure $ Con pos (c & idMeta .~ ty)
tcRho (Unboxed pos u) exp_ty = do
  tcUnboxed pos u exp_ty
  pure $ Unboxed pos u

tcUnboxed pos Int32{} = undefined

-------------------------------
-- inferSigma and checkSigma --
-------------------------------

inferSigma e = do
  exp_ty <- inferRho e
  env_tys <- getEnvTypes
  env_tvs <- getMetaTvs env_tys
  res_tvs <- getMetaTvs [exp_ty]
  let forall_tvs = res_tvs \\ env_tvs
  quantify forall_tvs exp_ty

chechSigma pos expr sigma = do
  (skol_tvs, rho) <- skolemise pos sigma
  checkRho expr rho
  env_tys <- getEnvTypes
  esc_tvs <- getFreeTyVars (sigma : env_tys)
  let bad_tvs = filter (`elem` esc_tvs) skol_tvs
  check (null bad_tvs) pos "Type not polymorphic enough"

--------------------------
-- Subsumption checking --
--------------------------

-- Invariant: if the secound argument is (Check rho),
--            then rho is in weak-prenex form
instSigma pos t1 (Check t2) = do
  unify pos t1 t2 
  pure t2
instSigma pos t1 (Infer r) = do
  t1' <- instantiate pos t1
  writeIORef r t1'
  pure t1'