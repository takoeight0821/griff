{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
module Language.Griff.Desugar where

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Lens
import           Data.Bifunctor
import qualified Data.Map                    as Map
import           Data.Maybe
import           Language.Griff.Core         as C
import           Language.Griff.Id
import           Language.Griff.Prelude
import qualified Language.Griff.Syntax       as S
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Infer (convertType)
import           Language.Griff.Typing.Monad

desugar :: (Carrier sig m, InferEff sig) => [S.Dec Id] -> m Toplevel
desugar ds = runReader (collectPureScDef scDefs) $ do
  ds' <- mapM dsScDef scDefs
  pure $ Toplevel { _scDef = ds'}
  where
    scDefs = mapMaybe (preview S._ScDef) ds
    collectPureScDef = mapMaybe collect
    collect (_, x, [], _) = Just x
    collect _             = Nothing

dsScDef :: (Carrier sig m, InferEff sig, Member (Reader [Id]) sig) => (a, Id, [Id], S.Exp Id) -> m (Id, NonEmpty Id, Exp)
dsScDef (_, f, [], e)   = do
  env <- get
  case Map.lookup f env of
    Nothing -> throwError $ UnboundVariable f
    Just (Forall xs ty) -> do
      addScheme (f, Forall xs $ TArr (TRecord mempty) ty)
      x <- newId "_"
      (f, x :| [],) <$> dsExp e
dsScDef (_, f, p:ps, e) = (f, p :| ps,) <$> dsExp e

dsExp :: (Carrier sig m, InferEff sig, Member (Reader [Id]) sig) => S.Exp Id -> m Exp
dsExp (S.Var _ x)    = do
  isPureScDef <- asks @[Id] (elem x)
  if isPureScDef
    then pure $ Apply (Var x) (Record [])
    else pure $ Var x
dsExp (S.Int _ x)    = pure $ Int x
dsExp (S.Char _ x)   = pure $ Char x
dsExp (S.String _ x) = pure $ String x
dsExp (S.Bool _ x)   = pure $ Bool x
dsExp (S.Record _ xs) = Record <$> mapM (secondM dsExp) xs
dsExp (S.Ascribe _ (S.Proj _ l v) t) = Proj l <$> dsExp v <*> pure (convertType t)
dsExp (S.Ascribe _ e _) = dsExp e
dsExp S.Proj{} = error "unreachable S.Proj"
dsExp (S.Apply _ e1 e2) = Apply <$> dsExp e1 <*> dsExp e2
dsExp (S.Lambda _ x e) = Lambda x <$> dsExp e
dsExp (S.Let _ f xs e1 e2) = do
  e1' <- dsExp e1
  Let f (foldr Lambda e1' xs) <$> dsExp e2
dsExp (S.LetRec _ xs e) =
  LetRec <$> mapM (\(f, p:ps, e1) -> (f, p :| ps,) <$> dsExp e1) xs <*> dsExp e
dsExp (S.BinOp _ o e1 e2) = BinOp o <$> mapM dsExp [e1, e2]
dsExp (S.Case _ v cs) =
  Case <$> dsExp v <*> mapM (bimapM (pure . dsPat) dsExp) cs
dsExp (S.If _ c t f) =
  Case <$> dsExp c <*> sequence [(BoolP True,) <$> dsExp t, (BoolP False,) <$> dsExp f]

dsPat :: S.Pat Id -> Pat
dsPat (S.VarP _ x) = VarP x
dsPat (S.RecordP _ xs) = RecordP $ map (second dsPat) xs
dsPat (S.VariantP _ label pat ty) =
  VariantP label (dsPat pat) (convertType ty)

dsTypeAliasDef :: (Id, [Id], S.Type Id) -> (Id, [Id], Ty)
dsTypeAliasDef (n, ps, t) = (n, ps, convertType t)
