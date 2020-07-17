{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.TcMonad where

import qualified Data.Map as Map
import Language.Griff.Id
import Language.Griff.MonadUniq
import Language.Griff.Prelude
import Language.Griff.Pretty
import Language.Griff.Rename (RnId)
import Language.Griff.TypeRep
import qualified Text.PrettyPrint.HughesPJ as P
import Text.Megaparsec.Pos (SourcePos)
import qualified Data.Text as Text

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
zonkType t@TyVar{} = pure t  
zonkType t@TyCon{} = pure t
zonkType t@PrimT{} = pure t
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
unify pos ty1 ty2
  | kind ty1 /= kind ty2 = errorOn pos $ "Kind mismatch:" <+> P.vcat [pPrint ty1, pPrint ty2]
  | badType ty1 || badType ty2 = errorOn pos $ "Panic! Unxepected types in unification:" <+> P.vcat [pPrint ty1, pPrint ty2] 

badType :: Type -> Bool
badType (TyVar (BoundTv _)) = True
badType _ = False