{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Griff.Core (Exp(..), Op(..), Pat(..), Toplevel(..), schemeOf, schemeOfPat) where

import           Control.Effect
import           Control.Effect.State
import           Control.Monad
import           Control.Monad.Fail
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Outputable
import           Data.Text
import           GHC.Generics
import           Language.Griff.Id
import           Language.Griff.Prelude
import           Language.Griff.Syntax       (Op (..))
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Infer (ConMap)
import           Language.Griff.Typing.Monad

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
         | BinOp Op [Exp]
         | Case Exp [(Pat, Exp)]
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
schemeOf (Var x) = generalize <$> get <*> lookup x
schemeOf Int{} = pure $ Forall [] (TPrim TInt)
schemeOf Char{} = pure $ Forall [] (TPrim TChar)
schemeOf String{} = pure $ Forall [] (TPrim TString)
schemeOf Bool{} = pure $ Forall [] (TPrim TBool)
schemeOf (Record xs) = do
  ts <- mapM (secondM (schemeOf >=> instantiate)) xs
  generalize <$> get <*> pure (TRecord $ Map.fromList ts)
schemeOf (Proj _ _ t) = generalize <$> get <*> pure t
schemeOf (Apply e1 _) = do
  TArr _ t <- instantiate =<< schemeOf e1
  generalize <$> get <*> pure t
schemeOf (Lambda x e) = do
  xt <- lookup x
  et <- instantiate =<< schemeOf e
  generalize <$> get <*> pure (TArr xt et)
schemeOf (Let _ _ e) = schemeOf e
schemeOf (LetRec _ e) = schemeOf e
schemeOf (Case _ ((_, e):_)) = schemeOf e
schemeOf Case{} = undefined
schemeOf (BinOp p _) = case p of
  Add -> pure $ Forall [] (TPrim TInt)
  Sub -> pure $ Forall [] (TPrim TInt)
  Mul-> pure $ Forall [] (TPrim TInt)
  Div -> pure $ Forall [] (TPrim TInt)
  Mod -> pure $ Forall [] (TPrim TInt)
  Eq  -> pure $ Forall [] (TPrim TBool)
  Neq -> pure $ Forall [] (TPrim TBool)
  Lt  -> pure $ Forall [] (TPrim TBool)
  Le  -> pure $ Forall [] (TPrim TBool)
  Gt  -> pure $ Forall [] (TPrim TBool)
  Ge  -> pure $ Forall [] (TPrim TBool)
  And -> pure $ Forall [] (TPrim TBool)
  Or  -> pure $ Forall [] (TPrim TBool)

schemeOfPat :: (Carrier sig f, Member (Error TypeError) sig, Member (State Env) sig, Member Fresh sig) => Pat -> f Scheme
schemeOfPat (VarP x) = generalize <$> get <*> lookup x
schemeOfPat (BoolP _) = pure $ Forall [] (TPrim TBool)
schemeOfPat (RecordP xs) = do
  ts <- mapM (secondM $ schemeOfPat >=> instantiate) xs
  generalize <$> get <*> pure (TRecord $ Map.fromList ts)
schemeOfPat (VariantP _ _ t) = generalize <$> get <*> pure t
