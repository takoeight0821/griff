{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
module Language.Griff.TypeRep where

import           Data.Data
import           Data.Map
import           Data.Text
import           GHC.Generics
import           Language.Griff.Id
import           Language.Griff.Prelude
import           Text.Show.Pretty

data Ty = TVar Id
        | TArr Ty Ty
        | TPrim TPrim
        | TRecord (Map Text Ty)
        | TVariant (Map Text Ty)
        | TCon Id [Ty]
  deriving (Eq, Ord, Show, Generic, Data)

instance PrettyVal Ty

data TPrim = TInt | TChar | TString | TBool
  deriving (Eq, Ord, Show, Generic, Data)

instance PrettyVal TPrim

data Scheme = Forall [Id] Ty
  deriving (Eq, Ord, Show, Generic, Data)

instance PrettyVal Scheme
