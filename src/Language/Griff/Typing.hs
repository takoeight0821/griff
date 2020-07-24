{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Typing where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Griff.Id
import Language.Griff.MonadUniq
import Language.Griff.Prelude
import Language.Griff.Pretty
-- import Language.Griff.Syntax hiding (Type (..))
-- import qualified Language.Griff.Syntax as S
import Language.Griff.TcEnv
import Language.Griff.TypeRep
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.PrettyPrint as P

---------------------------
-- Read and Write MetaTv --
---------------------------

newMetaTv :: (MonadUniq f, MonadIO f) => Kind -> f MetaTv
newMetaTv k = MetaTv <$> getUniq <*> pure k <*> newIORef Nothing

readMetaTv :: MonadIO m => MetaTv -> m (Maybe Type)
readMetaTv (MetaTv _ _ ref) = readIORef ref

writeMetaTv :: MonadIO m => MetaTv -> Type -> m ()
writeMetaTv (MetaTv _ k ref) t
  | k == kind t = writeIORef ref (Just t)
  | otherwise = errorDoc $ "Panic!" <+> "Kind of" <+> pPrint t <+> "is not" <+> pPrint k

-------------
-- Zonking --
-------------

zonkScheme :: MonadIO f => Scheme -> f Scheme
zonkScheme (Forall as t) = Forall as <$> zonkType t

zonkType :: MonadIO f => Type -> f Type
zonkType (TyMeta tv) = fromMaybe (TyMeta tv) <$> readMetaTv tv

--------------------------------
-- Generalize and Instantiate --
--------------------------------

generalize :: (MonadIO m, MonadUniq m) => TcEnv -> Type -> m Scheme
generalize env t = do
  fvs <- toList <$> freeMetaTvs env t
  as <- traverse (\tv -> newId (kind tv) (Name $ T.pack $ show tv)) fvs
  zipWithM_ writeMetaTv fvs (map TyVar as)
  Forall as <$> zonkType t

freeMetaTvs :: MonadIO m => TcEnv -> Type -> m (Set MetaTv)
freeMetaTvs env t = do
  env' <- traverse zonkScheme (view varEnv env)
  t' <- zonkType t
  pure $ metaTvs t' Set.\\ foldMap metaTvsScheme env'

metaTvs :: Type -> Set MetaTv
metaTvs (TyApp t1 t2) = metaTvs t1 <> metaTvs t2
metaTvs (TyArr t1 t2) = metaTvs t1 <> metaTvs t2
metaTvs (TyTuple ts) = mconcat $ map metaTvs ts
metaTvs (TyLazy t) = metaTvs t
metaTvs (TyMeta tv) = Set.singleton tv

metaTvsScheme :: Scheme -> Set MetaTv
metaTvsScheme (Forall _ t) = metaTvs t

instantiate :: (MonadUniq m, MonadIO m) => Scheme -> m Type
instantiate (Forall as t) = do
  vs <- traverse (\a -> TyMeta <$> newMetaTv (kind a)) as
  pure $ applySubst (Map.fromList $ zip as vs) t

applySubst :: Map TyVar Type -> Type -> Type
applySubst subst (TyVar v) = fromMaybe (TyVar v) $ Map.lookup v subst
applySubst subst (TyApp t1 t2) = TyApp (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyArr t1 t2) = TyArr (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyTuple ts) = TyTuple $ map (applySubst subst) ts
applySubst subst (TyLazy t) = TyLazy $ applySubst subst t
applySubst _ t = t

-----------------
-- Unification --
-----------------

unify :: MonadIO m => SourcePos -> Type -> Type -> m ()
unify _ (TyMeta tv1) (TyMeta tv2) | tv1 == tv2 = pure ()
unify pos (TyMeta tv) t = unifyMeta pos tv t
unify pos t (TyMeta tv) = unifyMeta pos tv t
unify pos (TyApp t11 t12) (TyApp t21 t22) = do
  unify pos t11 t21
  unify pos t12 t22
unify _ (TyVar v1) (TyVar v2) | v1 == v2 = pure ()

unifyMeta :: MonadIO m => SourcePos -> MetaTv -> Type -> m ()
unifyMeta pos tv t2
  | kind tv /= kind t2 = errorOn pos $ "Kind mismatch:" <+> P.vcat [P.quotes $ pPrint tv, pPrint t2]
  | otherwise = do
    mt1 <- readMetaTv tv
    case mt1 of
      Just t1 -> unify pos t1 t2
      Nothing -> do
        if tv `elem` metaTvs t2
          then errorOn pos $ "Occurs check" <+> P.quotes (pPrint tv) <+> "for" <+> pPrint t2
          else writeMetaTv tv t2