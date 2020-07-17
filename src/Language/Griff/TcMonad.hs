{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Type inference is inspired by Practical type inference for arbitrary-rank types
module Language.Griff.TcMonad where

import qualified Data.Map as Map
import Language.Griff.Id
import Language.Griff.MonadUniq
import Language.Griff.Prelude
import Language.Griff.Pretty
import Language.Griff.Rename (RnId)
import Language.Griff.TypeRep
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.PrettyPrint.HughesPJ as P

data TcEnv = TcEnv {_varEnv :: Map RnId Sigma}

makeLenses ''TcEnv

check :: Applicative f => Bool -> SourcePos -> Doc -> f ()
check True _ _ = pure ()
check False pos d = errorOn pos d

runTcM :: [(RnId, Sigma)] -> ReaderT TcEnv m a -> m a
runTcM binds m = runReaderT m (TcEnv {_varEnv = Map.fromList binds})

---------------------------------------
-- Dealing with the type environment --
---------------------------------------

extendVarEnv :: MonadReader TcEnv m => RnId -> Sigma -> m a -> m a
extendVarEnv var ty m = local (over varEnv (Map.insert var ty)) m

lookupVar :: MonadReader TcEnv m => SourcePos -> RnId -> m Sigma
lookupVar pos n = do
  env <- view varEnv
  case Map.lookup n env of
    Just ty -> pure ty
    Nothing -> errorOn pos ("Not in scope:" <+> P.quotes (pPrint n))

----------------------------------------
-- Creating, reading, writing MetaTvs --
----------------------------------------

newTyMeta :: (MonadUniq m, MonadIO m) => Kind -> m TyMeta
newTyMeta k = do
  u <- newId k "a"
  r <- newIORef Nothing
  pure $ TyMeta u r

newMetaTv :: (MonadUniq f, MonadIO f) => Kind -> f Type
newMetaTv k = MetaTv <$> newTyMeta k

newSkolemTyVar :: MonadUniq f => TyVar -> f TyVar
newSkolemTyVar tv = SkolemTv <$> newId (kind tv) (tyVarName tv)

readTv :: MonadIO m => TyMeta -> m (Maybe Tau)
readTv (TyMeta _ ref) = readIORef ref

writeTv :: MonadIO m => TyMeta -> Tau -> m ()
writeTv (TyMeta _ ref) ty = writeIORef ref (Just ty)

-------------------
-- Instantiation --
-------------------

instantiate :: (MonadUniq m, MonadIO m) => SourcePos -> Sigma -> m Rho
instantiate pos (Forall tvs ty) = do
  tvs' <- traverse (newMetaTv . kind) tvs
  pure $ substTy pos tvs tvs' ty
instantiate _ ty = pure ty

-- TODO: TyApp, TupleT, LazyTのskolemiseを考える
-- とりあえず、入れ子のForallはTyArrのパラメータにしか現れないとする
skolemise :: MonadUniq m => SourcePos -> Sigma -> m ([TyVar], Rho)
skolemise pos (Forall tvs ty) = do
  sks1 <- traverse newSkolemTyVar tvs
  (sks2, ty') <- skolemise pos (substTy pos tvs (map TyVar sks1) ty)
  pure (sks1 <> sks2, ty')
skolemise pos (TyArr t1 t2) = do
  (sks, t2') <- skolemise pos t2
  pure (sks, TyArr t1 t2')
skolemise _ t = pure ([], t)

--------------------
-- Quantification --
--------------------

quantify :: (MonadUniq m, MonadIO m) => [TyMeta] -> Rho -> m Sigma
quantify tvs ty = do
  new_bndrs <- traverse (\tv -> BoundTv <$> newId (kind tv) "a") tvs
  traverse_ bind (zip tvs new_bndrs)
  ty' <- zonkType ty
  pure $ Forall new_bndrs ty'
  where
    bind (tv, bndr) = writeTv tv $ TyVar bndr

-----------------------------
-- Getting the free tyvars --
-----------------------------

getEnvTypes :: MonadReader TcEnv f => f [Sigma]
getEnvTypes = Map.elems <$> view varEnv

getMetaTvs :: MonadIO m => [Sigma] -> m [TyMeta]
getMetaTvs tys = do
  tys' <- traverse zonkType tys
  pure $ metaTvs tys'

getFreeTyVars :: MonadIO m => [Sigma] -> m [TyVar]
getFreeTyVars tys = do
  tys' <- traverse zonkType tys
  pure $ freeTyVars tys'

-------------
-- Zonking --
-------------

zonkType :: MonadIO f => Sigma -> f Sigma
zonkType (Forall ns ty) = Forall ns <$> zonkType ty
zonkType (TyApp t1 t2) = TyApp <$> zonkType t1 <*> zonkType t2
zonkType t@TyVar {} = pure t
zonkType t@TyCon {} = pure t
zonkType t@PrimT {} = pure t
zonkType (TyArr t1 t2) = TyArr <$> zonkType t1 <*> zonkType t2
zonkType (TupleT ts) = TupleT <$> traverse zonkType ts
zonkType (LazyT t) = LazyT <$> zonkType t
zonkType (MetaTv tv) = do
  mb_ty <- readTv tv
  case mb_ty of
    Nothing -> pure (MetaTv tv)
    Just ty -> do
      ty' <- zonkType ty
      writeTv tv ty'
      pure ty'

-----------------
-- Unification --
-----------------

unify :: MonadIO m => SourcePos -> Tau -> Tau -> m ()
unify pos ty1 ty2
  | kind ty1 /= kind ty2 = errorOn pos $ "Kind mismatch:" <+> P.vcat [pPrint ty1, pPrint ty2]
  | badType ty1 || badType ty2 = errorOn pos $ "Panic! Unxepected types in unification:" <+> P.vcat [pPrint ty1, pPrint ty2]
unify pos (TyApp t11 t12) (TyApp t21 t22) = do
  unify pos t11 t21
  unify pos t12 t22
unify _ (TyVar tv1) (TyVar tv2) | tv1 == tv2 = pure ()
unify _ (TyCon c1) (TyCon c2) | c1 == c2 = pure ()
unify _ (PrimT t1) (PrimT t2) | t1 == t2 = pure ()
unify pos (TyArr arg1 res1) (TyArr arg2 res2) = do
  unify pos arg1 arg2
  unify pos res1 res2
unify pos (TupleT ts1) (TupleT ts2) = zipWithM_ (unify pos) ts1 ts2
unify pos (LazyT t1) (LazyT t2) = unify pos t1 t2
unify _ (MetaTv tv1) (MetaTv tv2) | tv1 == tv2 = pure ()
unify pos (MetaTv tv) ty = unifyVar pos tv ty
unify pos ty1 ty2 = errorOn pos $ "Cannot unify types:" <+> P.vcat [pPrint ty1, pPrint ty2]

-- Invariant: tv1 is a flexible type variable
unifyVar :: MonadIO m => SourcePos -> TyMeta -> Tau -> m ()
unifyVar pos tv1 ty2 = do
  -- Check whether tv1 is bound
  mb_ty1 <- readTv tv1
  case mb_ty1 of
    Just ty1 -> unify pos ty1 ty2
    Nothing -> unifyUnboundVar pos tv1 ty2

-- Invariant: the flexible type variable tv1 is not bound
unifyUnboundVar :: MonadIO m => SourcePos -> TyMeta -> Tau -> m ()
unifyUnboundVar pos tv1 ty2@(MetaTv tv2) = do
  -- We know that tv1 /= tv2 (else the top case in unify would catch it)
  mb_ty2 <- readTv tv2
  case mb_ty2 of
    Just ty2' -> unify pos (MetaTv tv1) ty2'
    Nothing -> writeTv tv1 ty2
unifyUnboundVar pos tv1 ty2 = do
  tvs2 <- getMetaTvs [ty2]
  if tv1 `elem` tvs2
    then occursCheckErr pos tv1 ty2
    else writeTv tv1 ty2

unifyTyArr :: (MonadUniq f, MonadIO f) => SourcePos -> Rho -> f (Sigma, Rho)
unifyTyArr _ (TyArr arg res) = pure (arg, res)
unifyTyArr pos tau = do
  arg <- newMetaTv Star
  res <- newMetaTv Star
  unify pos tau (arg `TyArr` res)
  pure (arg, res)

occursCheckErr :: SourcePos -> TyMeta -> Type -> a
occursCheckErr pos tv ty = errorOn pos $ "Occurs check for" <+> P.quotes (pPrint tv) <+> "in:" <+> pPrint ty

badType :: Type -> Bool
badType (TyVar (BoundTv _)) = True
badType _ = False