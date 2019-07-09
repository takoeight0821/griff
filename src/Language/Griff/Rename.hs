{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Language.Griff.Rename where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           Language.Griff.Id
import           Language.Griff.Monad
import           Language.Griff.Syntax

rename :: MonadIO m => [Dec Text] -> GriffT m [Dec Id]
rename ds = flip runReaderT Map.empty $ do
  env <- genTop $ map name ds
  local (env <>) $ mapM rnDec ds
  where
    name (ScSig _ x _)          = x
    name (ScDef _ x _ _)        = x
    name (TypeAliasDef _ x _ _) = x
    genTop [] = ask
    genTop (x : xs) =
      withName x $ const $ genTop xs

type Env = Map Text Id

type RnT m a = ReaderT Env (GriffT m) a

withNewName :: MonadIO m => Text -> RnT m a -> RnT m a
withNewName x m = do
  x' <- lift $ newId x
  local (Map.insert x x') m

withNewNames :: MonadIO m => [Text] -> RnT m a -> RnT m a
withNewNames xs m = do
  xs' <- mapM (lift . newId) xs
  local (Map.fromList (zip xs xs') <>) m

lookupName :: Monad m => Text -> RnT m (Maybe Id)
lookupName x =
  asks (Map.lookup x)

lookupName' :: Monad m => Text -> RnT m Id
lookupName' x = fromJust <$> lookupName x

withName :: MonadIO m => Text -> (Id -> RnT m a) -> RnT m a
withName x k = do
  x' <- asks (Map.lookup x)
  case x' of
    Nothing -> withNewName x $
      (fromJust <$> lookupName x) >>= k
    Just i -> k i

rnDec :: MonadIO m => Dec Text -> RnT m (Dec Id)
rnDec (ScSig s x t) =
  ScSig s <$> lookupName' x <*> withNewNames (Set.toList (freeTyVars t)) (rnType t)
rnDec (ScDef s x xs e) = withNewNames xs $
  ScDef s <$> lookupName' x <*> mapM lookupName' xs <*> rnExp e
rnDec (TypeAliasDef s x xs t) = withNewNames xs $
  TypeAliasDef s <$> lookupName' x <*> mapM lookupName' xs <*> rnType t

rnExp :: MonadIO m => Exp Text -> ReaderT Env (GriffT m) (Exp Id)
rnExp (Var s x) = Var s <$> lookupName' x
rnExp (Int s x) = pure $ Int s x
rnExp (Bool s x) = pure $ Bool s x
rnExp (Char s x) = pure $ Char s x
rnExp (String s x) = pure $ String s x
rnExp (Record s xs) = Record s <$> mapM (\(x, v) -> (x,) <$> rnExp v) xs
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
  env <- foldr (withNewName . view _1) ask xs
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

rnClause :: MonadIO m => (Pat Text, Exp Text) -> RnT m (Pat Id, Exp Id)
rnClause (pat, e) = do
  (pat', env) <- rnPat pat
  local (env <>) $ (pat',) <$> rnExp e

rnPat :: MonadIO m => Pat Text -> RnT m (Pat Id, Map Text Id)
rnPat (VarP s x) = withNewName x $ do
  x' <- lookupName' x
  pure (VarP s x', Map.fromList [(x, x')])
rnPat (IntP s x) = pure (IntP s x, Map.empty)
rnPat (CharP s x) = pure (CharP s x, Map.empty)
rnPat (StringP s x) = pure (StringP s x, Map.empty)
rnPat (ConstructorP s x ps) = do
  ps' <- mapM rnPat ps
  let pats = map fst ps'
  let env = foldr ((<>) . snd) Map.empty ps'
  x' <- lookupName' x
  pure (ConstructorP s x' pats, env)

freeTyVars :: Ord a => Type a -> Set.Set a
freeTyVars (TyApp _ _ ts)   = Set.unions $ map freeTyVars ts
freeTyVars (TyVar _ x)      = Set.singleton x
freeTyVars (TyArr _ t1 t2)  = freeTyVars t1 <> freeTyVars t2
freeTyVars TyPrim{}         = Set.empty
freeTyVars (TyRecord _ xs)  = Set.unions $ map (freeTyVars . snd) xs
freeTyVars (TyVariant _ xs) = Set.unions $ map (freeTyVars . snd) xs

rnType :: Monad m => Type Text -> RnT m (Type Id)
rnType (TyApp s con ts) = TyApp s <$> lookupName' con <*> mapM rnType ts
rnType (TyVar s x) = TyVar s <$> lookupName' x
rnType (TyArr s t1 t2) = TyArr s <$> rnType t1 <*> rnType t2
rnType (TyPrim s p) = pure $ TyPrim s p
rnType (TyRecord s xs) = TyRecord s <$> mapM (\(x, t) -> (x,) <$> rnType t) xs
rnType (TyVariant s xs) = TyVariant s <$> mapM (\(x, t) -> (x,) <$> rnType t) xs
