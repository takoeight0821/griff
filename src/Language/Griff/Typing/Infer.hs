{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
module Language.Griff.Typing.Infer where

import Text.Megaparsec.Pos
import           Control.Lens
import           Control.Monad.Except
import qualified Data.Map                    as Map
import           Data.Maybe
import qualified Data.Set                    as Set
import           Language.Griff.Id
import           Language.Griff.Syntax
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Monad
import           Language.Griff.Typing.Subst
import           Prelude                     hiding (lookup)

newtype ConMap f = ConMap { unConMap :: Map.Map Id (ConMap f -> [Ty] -> f Ty) }
  deriving (Semigroup, Monoid)

convertType :: MonadError TypeError f => ConMap f -> Type Id -> f Ty
convertType env (TyApp _ con args) =
  case Map.lookup con $ unConMap env of
    Nothing -> throwError $ UndefinedType con
    Just k  -> k env =<< mapM (convertType env) args
convertType _ (TyVar _ a) = pure $ TVar a
convertType env (TyArr _ t1 t2) = TArr <$> convertType env t1 <*> convertType env t2
convertType env (TyRecord _ xs) = TRecord . Map.fromList <$> mapM (\(x, t) -> (x,) <$> convertType env t) xs
convertType env (TyVariant _ xs) = TVariant . Map.fromList <$> mapM (\(x, t) -> (x,) <$> convertType env t) xs

toScheme :: Ty -> Scheme
toScheme ty = Forall (Set.toList $ ftv ty) ty

loadTypeAlias :: (MonadError TypeError m, Monad m) => (a, Id, [Id], Type Id) -> m (ConMap m)
loadTypeAlias (_, con, args, ty) = do
  let fun env xs = do
        ty' <- convertType env ty
        return $ apply (Subst $ Map.fromList $ zip args xs) ty'
  return $ ConMap $ Map.singleton con fun

loadScSig :: MonadInfer m => ConMap m -> (a, Id, Type Id) -> m Constraint
loadScSig env (_, x, t) = do
  tv <- lookup x
  (tv, ) <$> convertType env t

infer :: MonadInfer m => [Dec Id] -> m ()
infer ds = do
  let scSigs = mapMaybe scSig ds
  let scDefs = mapMaybe scDef ds
  let typeAliasDefs = mapMaybe typeAliasDef ds

  conMap <- mconcat <$> mapM loadTypeAlias typeAliasDefs

  prepare (map (view _2) scDefs)
  cs0 <- mapM (loadScSig conMap) scSigs
  update =<< apply <$> runSolve cs0

  (ts, cs1) <- inferDef scDefs
  sub <- runSolve cs1
  let scs = map (closeOver . apply sub) ts
  mapM_ addScheme (zip (map (view _2) scDefs) scs)
  update $ apply sub

  where
    scSig (ScSig s x t) = Just (s, x, t)
    scSig _             = Nothing
    scDef (ScDef s f xs e) = Just (s, f, xs, e)
    scDef _                = Nothing
    typeAliasDef (TypeAliasDef s con ps t) = Just (s, con, ps, t)
    typeAliasDef _                         = Nothing

    prepare = mapM_ (\x -> fresh >>= \tv -> addScheme (x, Forall [] tv))

inferDef :: MonadInfer m => [(SourcePos, Id, [Id], Exp Id)] -> m ([Ty], [(Ty, Ty)])
inferDef = undefined
