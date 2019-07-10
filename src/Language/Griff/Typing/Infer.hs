{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
module Language.Griff.Typing.Infer where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                    as Map
import           Data.Maybe
import qualified Data.Set                    as Set
import           Language.Griff.Id
import           Language.Griff.Syntax
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Monad
import           Language.Griff.Typing.Subst
import           Prelude                     hiding (lookup)
import           Text.Megaparsec.Pos

newtype ConMap f = ConMap { unConMap :: Map.Map Id (ConMap f -> [Ty] -> f Ty) }
  deriving (Semigroup, Monoid)

convertType :: MonadError TypeError f => ConMap f -> Type Id -> f Ty
convertType env (TyApp _ con args) =
  case Map.lookup con $ unConMap env of
    Nothing -> throwError $ UndefinedType con
    Just k  -> k env =<< mapM (convertType env) args
convertType _ (TyVar _ a) = pure $ TVar a
convertType env (TyArr _ t1 t2) = TArr <$> convertType env t1 <*> convertType env t2
convertType _ (TyPrim _ p) = pure $ TPrim p
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

  flip runReaderT conMap $ do
    (ts, cs1) <- second mconcat . unzip <$> mapM inferDef scDefs
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

inferDef :: MonadInfer m => (SourcePos, Id, [Id], Exp Id) -> ReaderT (ConMap m) m (Ty, [Constraint])
inferDef (s, f, xs, e) = do
  (t0, cs) <- inferExp $ foldr (Lambda s) e xs
  t1 <- lookup f
  return (t0, (t0, t1) : cs)

inferExp :: MonadInfer m => Exp Id -> ReaderT (ConMap m) m (Ty, [Constraint])
inferExp (Var _ a) = do
  t <- lookup a
  pure (t, [])
inferExp Int{} = pure (TPrim TInt, [])
inferExp Bool{} = pure (TPrim TBool, [])
inferExp Char{} = pure (TPrim TChar, [])
inferExp String{} = pure (TPrim TString, [])
inferExp (Record _ xs) = do
  (ts, cs) <- second mconcat . unzip <$> mapM (inferExp . snd) xs
  pure (TRecord $ Map.fromList (zip (map fst xs) ts), cs)
inferExp (Proj _ label _) = throwError $ UndecidableProj label
inferExp (Ascribe _ (Proj _ label e) t) = do
  env <- ask
  TVariant xs <- lift $ convertType env t
  let valType = fromJust $ Map.lookup label xs
  (eType, cs) <- inferExp e
  pure (TVariant xs, (valType, eType) : cs)
inferExp (Ascribe _ x t) = do
  (xt, cs) <- inferExp x
  env <- ask
  t' <- lift $ convertType env t
  pure (xt, (xt, t') : cs)
inferExp (Apply _ e1 e2) = do
  (e1Type, cs1) <- inferExp e1
  (e2Type, cs2) <- inferExp e2
  tv <- fresh
  pure (tv, (TArr e2Type tv, e1Type) : cs2 <> cs1)
inferExp (Lambda _ x e) = do
  tv <- fresh
  addScheme (x, Forall [] tv)
  (eType, cs) <- inferExp e
  pure (TArr tv eType, cs)
inferExp (Let s f xs e1 e2) = do
  env <- getEnv

  (t1, cs1) <- inferExp $ foldr (Lambda s) e1 xs
  sub <- runSolve cs1

  let sc = generalize (apply sub env) (apply sub t1)
  addScheme (f, sc)
  update (apply sub)

  (t2, cs2) <- inferExp e2
  pure (t2, cs1 <> cs2)
inferExp (LetRec s [(f, xs, e1)] e2) = do
  env <- getEnv

  tv <- fresh
  addScheme (f, Forall [] tv)

  (t0, cs0) <- inferExp $ foldr (Lambda s) e1 xs
  sub <- runSolve ((t0, tv) : cs0)

  let sc = generalize (apply sub env) (apply sub t0)
  addScheme (f, sc)
  update (apply sub)

  (t1, cs1) <- inferExp e2
  pure (t1, (t0, tv) : cs1 <> cs0)

ops :: MonadInfer m => Op -> m Ty
ops Add = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Sub = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Mul = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Div = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Mod = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Eq = do
  t <- fresh
  pure (t `TArr` (t `TArr` TPrim TBool))
ops Neq = do
  t <- fresh
  pure (t `TArr` (t `TArr` TPrim TBool))
ops Lt = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TBool))
ops Le = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TBool))
ops Gt = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TBool))
ops Ge = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TBool))
ops And = pure (TPrim TBool `TArr` (TPrim TBool `TArr` TPrim TBool))
ops Or = pure (TPrim TBool `TArr` (TPrim TBool `TArr` TPrim TBool))
