{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Language.Griff.Core (Exp(..), Op(..), Pat(..), Toplevel(..), schemeOf, schemeOfPat) where

import           Control.Effect
import           Control.Monad.Fail
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Outputable
import           Data.Text
import           GHC.Generics
import           Language.Griff.Id
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Infer (ConMap)
import           Language.Griff.Typing.Monad
import           Prelude                     hiding (lookup)

data Exp = Var Id
         | Int Integer
         | Char Char
         | String Text
         | Bool Bool
         | Record [(Text, Exp)]
         | Proj Text Exp Ty
         | Apply Exp Exp
         | Lambda Id Exp
         | Let Id Exp Exp
         | LetRec [(Id, Exp)] Exp
         | Prim Op [Exp]
         | Case Exp [(Pat, Exp)]
  deriving (Eq, Show, Generic, Outputable)

data Op = Add | Sub | Mul | Div | Mod | Eq | Neq
        | Lt | Le | Gt | Ge | And | Or
        | Error
  deriving (Eq, Show, Generic, Outputable)

data Pat = VarP Id
         | BoolP Bool
         | RecordP [(Text, Pat)]
         | VariantP Text Pat Ty
  deriving (Eq, Show, Generic, Outputable)

data Toplevel = Toplevel
  { _scDef   :: [(Id, Exp)]
  , _env     :: Map Id Scheme
  , _typeDef :: ConMap
  } deriving (Eq, Show, Generic, Outputable)

schemeOf :: (Carrier sig m, InferEff sig, MonadFail m) => Exp -> m Scheme
schemeOf (Var x) = generalize <$> getEnv <*> lookup x
schemeOf Int{} = pure $ Forall [] (TPrim TInt)
schemeOf Char{} = pure $ Forall [] (TPrim TChar)
schemeOf String{} = pure $ Forall [] (TPrim TString)
schemeOf Bool{} = pure $ Forall [] (TPrim TBool)
schemeOf (Record xs) = do
  ts <- mapM (\(n, v) -> (n,) <$> (instantiate =<< schemeOf v)) xs
  generalize <$> getEnv <*> pure (TRecord $ Map.fromList ts)
schemeOf (Proj _ _ t) = generalize <$> getEnv <*> pure t
schemeOf (Apply e1 _) = do
  TArr _ t <- instantiate =<< schemeOf e1
  generalize <$> getEnv <*> pure t
schemeOf (Lambda x e) = do
  xt <- lookup x
  et <- instantiate =<< schemeOf e
  generalize <$> getEnv <*> pure (TArr xt et)
schemeOf (Let _ _ e) = schemeOf e
schemeOf (LetRec _ e) = schemeOf e
schemeOf (Case _ ((_, e):_)) = schemeOf e
schemeOf Case{} = undefined
schemeOf (Prim op _) = case op of
  Add -> pure $ Forall [] (TPrim TInt)
  Sub -> pure $ Forall [] (TPrim TInt)
  Mul-> pure $ Forall [] (TPrim TInt)
  Div -> pure $ Forall [] (TPrim TInt)
  Mod -> pure $ Forall [] (TPrim TInt)
  Eq -> pure $ Forall [] (TPrim TBool)
  Neq -> pure $ Forall [] (TPrim TBool)
  Lt -> pure $ Forall [] (TPrim TBool)
  Le -> pure $ Forall [] (TPrim TBool)
  Gt -> pure $ Forall [] (TPrim TBool)
  Ge -> pure $ Forall [] (TPrim TBool)
  And -> pure $ Forall [] (TPrim TBool)
  Or -> pure $ Forall [] (TPrim TBool)
  Error -> do
    a <- newId "a"
    pure $ Forall [a] (TVar a)

schemeOfPat :: (Carrier sig f, Member (Error TypeError) sig, Member (State Env) sig, Member Fresh sig) => Pat -> f Scheme
schemeOfPat (VarP x) = generalize <$> getEnv <*> lookup x
schemeOfPat (BoolP _) = pure $ Forall [] (TPrim TBool)
schemeOfPat (RecordP xs) = do
  ts <- mapM (\(n, p) -> (n,) <$> (instantiate =<< schemeOfPat p)) xs
  generalize <$> getEnv <*> pure (TRecord $ Map.fromList ts)
schemeOfPat (VariantP _ _ t) = generalize <$> getEnv <*> pure t
