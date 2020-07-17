{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Infer where

import Data.List ((\\))
import Language.Griff.Id
import Language.Griff.MonadUniq
import Language.Griff.Prelude
import Language.Griff.Syntax as S
import Language.Griff.Pretty
import Language.Griff.TcMonad
import Language.Griff.TypeRep as T
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.PrettyPrint.HughesPJ as P

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
  expr' <- tcRho expr (Infer ref)
  (expr',) <$> readIORef ref

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

tcUnboxed :: (MonadIO m, MonadUniq m) => SourcePos -> Unboxed -> Expected Tau -> m Tau
tcUnboxed pos Int32 {} exp_ty = instSigma pos (PrimT Int32T) exp_ty
tcUnboxed pos Int64 {} exp_ty = instSigma pos (PrimT Int64T) exp_ty
tcUnboxed pos Float {} exp_ty = instSigma pos (PrimT FloatT) exp_ty
tcUnboxed pos Double {} exp_ty = instSigma pos (PrimT CharT) exp_ty
tcUnboxed pos String {} exp_ty = instSigma pos (PrimT StringT) exp_ty

-------------------------------
-- inferSigma and checkSigma --
-------------------------------

inferSigma e = do
  (e', exp_ty) <- inferRho e
  env_tys <- getEnvTypes
  env_tvs <- getMetaTvs env_tys
  res_tvs <- getMetaTvs [exp_ty]
  let forall_tvs = res_tvs \\ env_tvs
  (e',) <$> quantify forall_tvs exp_ty

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

subsCheck :: (MonadUniq m, MonadIO m) => SourcePos -> Sigma -> Sigma -> m ()
subsCheck pos sigma1 sigma2 = do
  (skol_tvs, rho2) <- skolemise pos sigma2
  subsCheckRho pos sigma1 rho2
  esc_tvs <- getFreeTyVars [sigma1, sigma2]
  let bad_tvs = filter (`elem` esc_tvs) skol_tvs
  check (null bad_tvs) pos $
    P.vcat
      [ "Subsumption check failed:",
        P.nest 2 (pPrint sigma1),
        "is not as polymorphic as",
        P.nest 2 (pPrint sigma2)
      ]

subsCheckRho :: (MonadUniq m, MonadIO m) => SourcePos -> Sigma -> Rho -> m ()
subsCheckRho pos sigma1@(Forall _ _) rho2 = do
  rho1 <- instantiate pos sigma1
  subsCheckRho pos rho1 rho2
subsCheckRho pos rho1 (T.TyArr a2 r2) = do
  (a1, r1) <- unifyTyArr pos rho1
  subsCheckTyArr pos a1 r1 a2 r2
subsCheckRho pos (T.TyArr a1 r1) rho2 = do
  (a2, r2) <- unifyTyArr pos rho2
  subsCheckTyArr pos a1 r1 a2 r2
subsCheckRho pos tau1 tau2 = unify pos tau1 tau2

subsCheckTyArr :: (MonadUniq m, MonadIO m) => SourcePos -> Sigma -> Rho -> Sigma -> Rho -> m ()
subsCheckTyArr pos a1 r1 a2 r2 = do
  subsCheck pos a2 a1
  subsCheckRho pos r1 r2

-- Invariant: if the secound argument is (Check rho),
--            then rho is in weak-prenex form
instSigma :: (MonadUniq m, MonadIO m) => SourcePos -> Sigma -> Expected Rho -> m Rho
instSigma pos t1 (Check t2) = do
  subsCheckRho pos t1 t2
  pure t2
instSigma pos t1 (Infer r) = do
  t1' <- instantiate pos t1
  writeIORef r t1'
  pure t1'