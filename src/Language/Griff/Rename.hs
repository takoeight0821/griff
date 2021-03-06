{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Rename where

import Data.List.Predicate (allUnique)
import qualified Data.Map as Map
import Language.Griff.Id
import Language.Griff.MonadUniq
import Language.Griff.Prelude
import Language.Griff.Pretty
import Language.Griff.RnEnv
import Language.Griff.Syntax
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.PrettyPrint as P

lookupVarName :: MonadReader RnEnv m => SourcePos -> Name -> m RnId
lookupVarName pos name = do
  vm <- view varEnv
  case vm ^. at name of
    Just name' -> pure name'
    Nothing -> errorOn pos $ "Not in scope:" <+> P.quotes (pPrint name)

lookupTypeName :: MonadReader RnEnv m => SourcePos -> Name -> m RnId
lookupTypeName pos name = do
  tm <- view typeEnv
  case tm ^. at name of
    Just name' -> pure name'
    Nothing -> errorOn pos $ "Not in scope:" <+> P.quotes (pPrint name)

rename :: MonadUniq m => RnState -> RnEnv -> [Decl (Griff 'Parse)] -> m [Decl (Griff 'Rename)]
rename rnState rnEnv ds =
  evalStateT ?? rnState $
    runReaderT ?? rnEnv $
      rnDecls ds

-- renamer

rnDecls :: (MonadUniq m, MonadReader RnEnv m, MonadState RnState m) => [Decl (Griff 'Parse)] -> m [Decl (Griff 'Rename)]
rnDecls ds = do
  -- RnEnvの生成
  let (varNames, typeNames) = toplevelIdents ds
  vm <- mconcat <$> traverse (\v -> Map.singleton v <$> newId NoMeta v) varNames
  tm <- mconcat <$> traverse (\v -> Map.singleton v <$> newId NoMeta v) typeNames
  let rnEnv = RnEnv vm tm
  -- RnStateの生成
  --   定義されていない識別子に対するInfixはエラー
  local (rnEnv <>) $ do
    rnState <- RnState <$> infixDecls ds
    -- 生成したRnEnv, RnStateの元でtraverse rnDecl ds
    put rnState
    traverse rnDecl ds

-- Declで定義されるトップレベル識別子はすでにRnEnvに正しく登録されているとする
-- infix宣言はすでに解釈されRnStateに登録されているとする
rnDecl :: (MonadUniq m, MonadReader RnEnv m, MonadState RnState m) => Decl (Griff 'Parse) -> m (Decl (Griff 'Rename))
rnDecl (ScDef pos name params expr) = do
  params' <- traverse (newId NoMeta) params
  local (over varEnv (Map.fromList (zip params params') <>)) $
    ScDef pos <$> lookupVarName pos name <*> pure params' <*> rnExp expr
rnDecl (ScSig pos name typ) = ScSig pos <$> lookupVarName pos name <*> rnType typ
rnDecl (DataDef pos name params cs) = do
  params' <- traverse (newId NoMeta) params
  local (over typeEnv (Map.fromList (zip params params') <>)) $
    DataDef pos <$> lookupTypeName pos name <*> pure params' <*> traverse (bitraverse (lookupVarName pos) (traverse rnType)) cs
rnDecl (Infix pos assoc prec name) = Infix pos assoc prec <$> lookupVarName pos name
rnDecl (Forign pos name@(Name raw) typ) = Forign (pos, raw) <$> lookupVarName pos name <*> rnType typ

-- 名前解決の他に，infix宣言に基づくOpAppの再構成も行う
rnExp :: (MonadReader RnEnv m, MonadState RnState m, MonadUniq m) => Exp (Griff 'Parse) -> m (Exp (Griff 'Rename))
rnExp (Var pos name) = Var pos <$> lookupVarName pos name
rnExp (Con pos name) = Con pos <$> lookupVarName pos name
rnExp (Unboxed pos val) = pure $ Unboxed pos val
rnExp (Apply pos e1 e2) = Apply pos <$> rnExp e1 <*> rnExp e2
rnExp (OpApp pos op e1 e2) = do
  op' <- lookupVarName pos op
  e1' <- rnExp e1
  e2' <- rnExp e2
  fixity <- Map.lookup op' <$> use infixInfo
  case fixity of
    Just fixity -> pure $ mkOpApp pos fixity op' e1' e2'
    Nothing -> errorOn pos $ "No infix declaration:" <+> P.quotes (pPrint op)
rnExp (Fn pos cs) = Fn pos <$> traverse rnClause cs
rnExp (Tuple pos es) = Tuple pos <$> traverse rnExp es
rnExp (Force pos e) = Force pos <$> rnExp e

rnType :: MonadReader RnEnv m => Type (Griff 'Parse) -> m (Type (Griff 'Rename))
rnType (TyApp pos t ts) = TyApp pos <$> rnType t <*> traverse rnType ts
rnType (TyVar pos x) = TyVar pos <$> lookupTypeName pos x
rnType (TyCon pos x) = TyCon pos <$> lookupTypeName pos x
rnType (TyArr pos t1 t2) = TyArr pos <$> rnType t1 <*> rnType t2
rnType (TyTuple pos ts) = TyTuple pos <$> traverse rnType ts
rnType (TyLazy pos t) = TyLazy pos <$> rnType t

rnClause :: (MonadUniq m, MonadReader RnEnv m, MonadState RnState m) => Clause (Griff 'Parse) -> m (Clause (Griff 'Rename))
rnClause (Clause pos ps e) = do
  let vars = concatMap patVars ps

  -- varsに重複がないことを確認
  when (not $ allUnique vars) $
    errorOn pos "Same variables occurs in a pattern"

  vars' <- traverse (newId NoMeta) vars
  let vm = Map.fromList $ zip vars vars'
  local (over varEnv (vm <>)) $ Clause pos <$> traverse rnPat ps <*> rnExp e
  where
    patVars (VarP _ x) = [x]
    patVars (ConP _ _ xs) = concatMap patVars xs
    patVars UnboxedP {} = []

rnPat :: MonadReader RnEnv m => Pat (Griff 'Parse) -> m (Pat (Griff 'Rename))
rnPat (VarP pos x) = VarP pos <$> lookupVarName pos x
rnPat (ConP pos x xs) = ConP pos <$> lookupVarName pos x <*> traverse rnPat xs
rnPat (UnboxedP pos x) = pure $ UnboxedP pos x

-- トップレベル識別子を列挙
toplevelIdents :: [Decl (Griff 'Parse)] -> ([Name], [Name])
toplevelIdents ds = (ordNub $ concatMap f ds, ordNub $ concatMap g ds)
  where
    f (ScDef _ x _ _) = [x]
    f (ScSig _ x _) = [x]
    f (DataDef _ _ _ xs) = map fst xs
    f (Forign _ x _) = [x]
    f _ = []
    g (DataDef _ x _ _) = [x]
    g _ = []

-- infix宣言をMapに変換
infixDecls :: MonadReader RnEnv m => [Decl (Griff 'Parse)] -> m (Map RnId (Assoc, Int))
infixDecls ds = mconcat <$> traverse f ds
  where
    f (Infix pos assoc order name) = do
      name' <- lookupVarName pos name
      pure $ Map.singleton name' (assoc, order)
    f _ = pure mempty

mkOpApp :: SourcePos -> (Assoc, Int) -> RnId -> Exp (Griff 'Rename) -> Exp (Griff 'Rename) -> Exp (Griff 'Rename)
-- (e11 op1 e12) op2 e2
mkOpApp pos2 fix2 op2 (OpApp (pos1, fix1) op1 e11 e12) e2
  | nofix_error =
    errorOn pos1 $
      "Precedence parsing error:"
        P.$+$ P.nest
          2
          ( "cannot mix" <+> P.quotes (pPrint op1) <+> P.brackets (pPrint fix1)
              <+> "and"
              <+> P.quotes (pPrint op2)
              <+> P.brackets (pPrint fix2)
              <+> "in the same infix expression"
          )
  | associate_right = OpApp (pos1, fix1) op1 e11 (OpApp (pos2, fix2) op2 e12 e2)
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2
mkOpApp pos fix op e1 e2 = OpApp (pos, fix) op e1 e2

compareFixity :: (Assoc, Int) -> (Assoc, Int) -> (Bool, Bool)
compareFixity (assoc1, prec1) (assoc2, prec2) =
  case prec1 `compare` prec2 of
    GT -> left
    LT -> right
    EQ -> case (assoc1, assoc2) of
      (RightA, RightA) -> right
      (LeftA, LeftA) -> left
      _ -> error_please
  where
    right = (False, True)
    left = (False, False)
    error_please = (True, False)
