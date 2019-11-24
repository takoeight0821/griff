{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Language.Griff.Typing.Infer (infer, convertType) where

import           Control.Effect.Error
import           Control.Effect.State
import           Control.Lens                hiding (op)
import           Control.Monad
import           Control.Monad.Fail
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Tuple.Extra            (uncurry3)
import           Language.Griff.Id
import           Language.Griff.Prelude
import           Language.Griff.Syntax
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Monad
import           Language.Griff.Typing.Subst
import           Text.Megaparsec.Pos

convertType :: Type Id -> Ty
convertType (TyApp _ con args) = TCon con $ map convertType args
convertType (TyVar _ a) = TVar a
convertType (TyArr _ t1 t2) = TArr (convertType t1) (convertType t2)
convertType (TyPrim _ p) = TPrim p
convertType (TyRecord _ xs) = TRecord $ Map.fromList $ map (second convertType) xs
convertType (TyVariant _ xs) = TVariant $ Map.fromList $ map (second convertType) xs

loadTypeAlias :: (a, Id, [Id], Type Id) -> ConMap
loadTypeAlias (_, con, args, ty) = ConMap $ Map.singleton con (args, convertType ty)

loadScSig :: InferEff sig m => (a, Id, Type Id) -> m Constraint
loadScSig (_, x, t) = do
  tv <- lookup x
  pure (tv, convertType t)

infer :: (InferEff sig m, MonadFail m) => [Dec Id] -> m ()
infer ds = do
  let scSigs = mapMaybe (preview _ScSig) ds
  let scDefs = mapMaybe (preview _ScDef) ds
  let typeAliasDefs = mapMaybe (preview _TypeAliasDef) ds

  let conMap = mconcat $ map loadTypeAlias typeAliasDefs
  put @ConMap conMap

  prepare (map (view _2) scDefs)
  cs0 <- mapM loadScSig scSigs
  modify @Env =<< apply <$> runSolve cs0

  (ts, cs1) <- second mconcat <$> mapAndUnzipM inferDef scDefs
  sub <- runSolve cs1
  let scs = map (closeOver . apply sub) ts
  mapM_ addScheme (zip (map (view _2) scDefs) scs)
  modify @Env $ apply sub
  where
    prepare = mapM_ (\x -> newMeta >>= \tv -> addScheme (x, Forall [] tv))

inferDef :: (InferEff sig m, MonadFail m) => (SourcePos, Id, [Id], Exp Id) -> m (Ty, [Constraint])
inferDef (s, f, xs, e) = do
  (t0, cs) <- inferExp $ foldr (Lambda s) e xs
  t1 <- lookup f
  return (t0, (t0, t1) : cs)

inferExp :: (InferEff sig m, MonadFail m) => Exp Id -> m (Ty, [Constraint])
inferExp (Var _ a) = do
  t <- lookup a
  pure (t, [])
inferExp Int{} = pure (TPrim TInt, [])
inferExp Bool{} = pure (TPrim TBool, [])
inferExp Char{} = pure (TPrim TChar, [])
inferExp String{} = pure (TPrim TString, [])
inferExp (Record _ xs) = do
  (ts, cs) <- second mconcat <$> mapAndUnzipM inferExp values
  pure (TRecord $ Map.fromList (zip labels ts), cs)
  where
    labels = map fst xs
    values = map snd xs
inferExp (Proj _ label _) = throwError $ UndecidableProj label
inferExp (Ascribe _ (Proj _ label e) t) = do
  TVariant xs <- expandTCon $ convertType t
  let valType = fromJust $ Map.lookup label xs
  (eType, cs) <- inferExp e
  pure (convertType t, (valType, eType) : cs)
inferExp (Ascribe _ x t) = do
  (xt, cs) <- inferExp x
  pure (xt, (xt, convertType t) : cs)
inferExp (Apply _ e1 e2) = do
  (e1Type, cs1) <- inferExp e1
  (e2Type, cs2) <- inferExp e2
  tv <- newMeta
  pure (tv, (TArr e2Type tv, e1Type) : cs2 <> cs1)
inferExp (Lambda _ x e) = do
  tv <- newMeta
  addScheme (x, Forall [] tv)
  (eType, cs) <- inferExp e
  pure (TArr tv eType, cs)
inferExp (Let s f xs e1 e2) = do
  env <- get
  (t1, cs1) <- inferExp $ foldr (Lambda s) e1 xs
  letVar env f t1 cs1
  (t2, cs2) <- inferExp e2
  pure (t2, cs1 <> cs2)
inferExp (LetRec s ds e2) = do
  env <- get
  mapM_ (prepare . view _1) ds
  cs0 <- concat <$> mapM (uncurry3 $ inferRec env) ds
  (t1, cs1) <- inferExp e2
  pure (t1, cs1 <> cs0)
  where
    prepare f = newMeta >>= \tv -> addScheme (f, Forall [] tv)
    inferRec env f xs e = do
      (t0, cs0) <- inferExp $ foldr (Lambda s) e xs
      tv <- lookup f
      letVar env f t0 $ (t0, tv) : cs0
      pure ((t0, tv) : cs0)
inferExp (BinOp _ op e1 e2) = do
  (e1t, e1cs) <- inferExp e1
  (e2t, e2cs) <- inferExp e2
  opt <- ops op
  ret <- newMeta
  pure (ret, (opt, e1t `TArr` (e2t `TArr` ret)) : e1cs <> e2cs)
inferExp (If _ c t f) = do
  (ct, ccs) <- inferExp c
  (tt, tcs) <- inferExp t
  (ft, fcs) <- inferExp f
  pure (tt, [(ct, TPrim TBool), (tt, ft)] <> ccs <> tcs <> fcs)
inferExp (Case _ e clauses) = do
  (et, ecs) <- inferExp e
  xs <- mapM (inferClause et) clauses
  let t:ts = map fst xs -- 空のcaseは想定しない
  let cs0 = concatMap snd xs
  let cs1 = map (t,) ts
  pure (t, cs1 <> cs0 <> ecs)

inferClause :: (InferEff sig m, MonadFail m) => Ty -> (Pat Id, Exp Id) -> m (Ty, [Constraint])
inferClause t0 (pat, e) = do
  cs0 <- inferPat t0 pat
  (eTy, cs1) <- inferExp e
  pure (eTy, cs1 <> cs0)

inferPat :: (InferEff sig m, MonadFail m) => Ty -> Pat Id -> m [Constraint]
inferPat t0 (VarP _ x) = do
  tv <- newMeta
  addScheme (x, Forall [] tv)
  pure [(t0, tv)]
inferPat t0 (RecordP _ xs) = do
  ts <- mapM (const newMeta) xs
  let ty = TRecord $ Map.fromList $ zip (map fst xs) ts
  cs <- concat <$> zipWithM inferPat ts (map snd xs)
  pure $ (t0, ty) : cs
inferPat t0 (VariantP _ label x ty) = do
  TVariant xs <- expandTCon $ convertType ty
  let Just valType = Map.lookup label xs
  cs <- inferPat valType x
  pure $ (t0, convertType ty) : cs

ops :: InferEff sig f => Op -> f Ty
ops Add = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Sub = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Mul = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Div = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Mod = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TInt))
ops Eq = do
  t <- newMeta
  pure (t `TArr` (t `TArr` TPrim TBool))
ops Neq = do
  t <- newMeta
  pure (t `TArr` (t `TArr` TPrim TBool))
ops Lt = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TBool))
ops Le = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TBool))
ops Gt = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TBool))
ops Ge = pure (TPrim TInt `TArr` (TPrim TInt `TArr` TPrim TBool))
ops And = pure (TPrim TBool `TArr` (TPrim TBool `TArr` TPrim TBool))
ops Or = pure (TPrim TBool `TArr` (TPrim TBool `TArr` TPrim TBool))

letVar :: InferEff sig m => Env -> Id -> Ty -> [Constraint] -> m ()
letVar env var ty cs = do
  sub <- runSolve cs
  let sc = generalize (apply sub env) (apply sub ty)
  addScheme (var, sc)
  modify @Env (apply sub)
