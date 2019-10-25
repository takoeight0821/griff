{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
module Language.Griff.Rename (rename) where

import           Control.Effect
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Lens
import           Control.Monad
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Set               as Set
import           Data.Text              (Text)
import           Language.Griff.Id
import           Language.Griff.Prelude
import           Language.Griff.Syntax

newtype TyVarEnv = TyVarEnv { _tyvarBinderMap :: Map Id (Map Text Id) }
makeLenses ''TyVarEnv

instance Semigroup TyVarEnv where
  (TyVarEnv e1) <> (TyVarEnv e2) = TyVarEnv $ e1 <> e2

data Env = Env { _nameMap  :: Map Text Id -- variables, functions, type constructors
               , _tyvarMap :: Map Text Id -- type variables
               } deriving (Show)
makeLenses ''Env

instance Semigroup Env where
  (Env n1 t1) <> (Env n2 t2) = Env (n1 <> n2) (t1 <> t2)

rename :: (Carrier sig m, Effect sig, Member Fresh sig) => [Dec Text] -> m [Dec Id]
rename ds = evalState (TyVarEnv mempty) $ runReader (Env mempty mempty) $ do
  env <- genTop $ map name ds
  local (const env) $ mapM rnDec ds
  where
    name (ScSig _ x _)          = x
    name (ScDef _ x _ _)        = x
    name (TypeAliasDef _ x _ _) = x

genTop :: (Carrier sig m, Foldable t, Member (Reader Env) sig, Member Fresh sig) => t Text -> m Env
genTop = foldr withNewName (ask @Env)

withNewName :: (Carrier sig m, Member (Reader Env) sig, Member Fresh sig) => Text -> m b -> m b
withNewName x m = do
  x' <- newId x
  local (over nameMap $ Map.insert x x') m

withNewNames :: (Carrier sig m, Member (Reader Env) sig, Member Fresh sig) => [Text] -> m b -> m b
withNewNames xs m = do
  xs' <- mapM newId xs
  local (over nameMap (Map.fromList (zip xs xs') <>)) m

withNewTyvars :: (Carrier sig m, Member (Reader Env) sig, Member Fresh sig) => [Text] -> m b -> m b
withNewTyvars xs m = do
  xs' <- mapM newId xs
  local (over tyvarMap (Map.fromList (zip xs xs') <>)) m

lookupName :: (Carrier sig m, Member (Reader Env) sig) => Text -> m (Maybe Id)
lookupName x = asks (Map.lookup x . view nameMap)

lookupName' :: (Carrier sig m, Member (Reader Env) sig) => Text -> m Id
lookupName' x = fromMaybe (error (show x <> " is not defined (name)")) <$> lookupName x

lookupTyvar :: (Carrier sig m, Member (Reader Env) sig) => Text -> m (Maybe Id)
lookupTyvar x = asks (Map.lookup x . view tyvarMap)

lookupTyvar' :: (Carrier sig f, Member (Reader Env) sig) => Text -> f Id
lookupTyvar' x = fromMaybe (error (show x <> " is not defined (tyvar)")) <$> lookupTyvar x

rnDec :: (Carrier sig m, Member (Reader Env) sig, Member Fresh sig, Member (State TyVarEnv) sig) => Dec Text -> m (Dec Id)
rnDec (ScSig s x t) = do
  x' <- lookupName' x

  let fvs = Set.toList $ freeTyVars t
  fvEnv <- Map.fromList . zip fvs <$> mapM newId fvs

  modify (over tyvarBinderMap $ Map.insert x' fvEnv)

  local (over tyvarMap (fvEnv <>)) $
    ScSig s x' <$> rnType t
rnDec (ScDef s x xs e) = withNewNames xs $ do
  x' <- lookupName' x
  fvEnv <- gets (fromMaybe mempty . Map.lookup x' . view tyvarBinderMap)
  local (over tyvarMap (fvEnv <>)) $
    ScDef s x' <$> mapM lookupName' xs <*> rnExp e
rnDec (TypeAliasDef s x xs t) = withNewTyvars xs $
  TypeAliasDef s <$> lookupName' x <*> mapM lookupTyvar' xs <*> rnType t

rnExp :: (Carrier sig f, Member (Reader Env) sig, Member (State TyVarEnv) sig, Member Fresh sig) => Exp Text -> f (Exp Id)
rnExp (Var s x) = Var s <$> lookupName' x
rnExp (Int s x) = pure $ Int s x
rnExp (Bool s x) = pure $ Bool s x
rnExp (Char s x) = pure $ Char s x
rnExp (String s x) = pure $ String s x
rnExp (Record s xs) = Record s <$> mapM (secondM rnExp) xs
rnExp (Proj s x v) = Proj s x <$> rnExp v
rnExp (Ascribe s e t) = Ascribe s <$> rnExp e <*> rnType t
rnExp (Apply s e1 e2) = Apply s <$> rnExp e1 <*> rnExp e2
rnExp (Lambda s x e) = withNewName x $
  Lambda s <$> lookupName' x <*> rnExp e
rnExp (Let s f xs e1 e2) = withNewName f $ do
  e2' <- rnExp e2
  withNewNames xs $
    Let s <$> lookupName' f <*> mapM lookupName' xs <*> rnExp e1 <*> pure e2'
rnExp (LetRec s xs body) = do
  env :: Env <- foldr (withNewName . view _1) ask xs
  local (env <>) $
    LetRec s
      <$> forM xs (\(f, ps, e) ->
                     withNewNames ps $ (,,) <$> lookupName' f <*> mapM lookupName' ps <*> rnExp e)
      <*> rnExp body
rnExp (BinOp s o e1 e2) =
  BinOp s o <$> rnExp e1 <*> rnExp e2
rnExp (Case s e cs) =
  Case s <$> rnExp e <*> mapM rnClause cs
rnExp (If s c t f) = If s <$> rnExp c <*> rnExp t <*> rnExp f

rnClause :: (Carrier sig m, Member (Reader Env) sig, Member Fresh sig, Member (State TyVarEnv) sig) => (Pat Text, Exp Text) -> m (Pat Id, Exp Id)
rnClause (pat, e) = do
  (pat', env) <- rnPat pat
  local (over nameMap (env <>)) $ (pat',) <$> rnExp e

rnPat :: (Carrier sig m, Member (Reader Env) sig, Member Fresh sig) => Pat Text -> m (Pat Id, Map Text Id)
rnPat (VarP s x) = withNewName x $ do
  x' <- lookupName' x
  pure (VarP s x', Map.fromList [(x, x')])
rnPat (RecordP s xs) = do
  (ps, ns) <- unzip <$> mapM (rnPat . snd) xs
  pure (RecordP s (zip (map fst xs) ps), Map.unions ns)
rnPat (VariantP s x p ty) = do
  (p', n) <- rnPat p
  ty' <- rnType ty
  pure (VariantP s x p' ty', n)

freeTyVars :: Ord a => Type a -> Set.Set a
freeTyVars (TyApp _ _ ts)   = Set.unions $ map freeTyVars ts
freeTyVars (TyVar _ x)      = Set.singleton x
freeTyVars (TyArr _ t1 t2)  = freeTyVars t1 <> freeTyVars t2
freeTyVars TyPrim{}         = Set.empty
freeTyVars (TyRecord _ xs)  = Set.unions $ map (freeTyVars . snd) xs
freeTyVars (TyVariant _ xs) = Set.unions $ map (freeTyVars . snd) xs

rnType :: (Carrier sig f, Member (Reader Env) sig) => Type Text -> f (Type Id)
rnType (TyApp s con ts) = TyApp s <$> lookupName' con <*> mapM rnType ts
rnType (TyVar s x)      = TyVar s <$> lookupTyvar' x
rnType (TyArr s t1 t2)  = TyArr s <$> rnType t1 <*> rnType t2
rnType (TyPrim s p)     = pure $ TyPrim s p
rnType (TyRecord s xs)  = TyRecord s <$> mapM (secondM rnType) xs
rnType (TyVariant s xs) = TyVariant s <$> mapM (secondM rnType) xs
