{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Griff.TypeRep where

import           Data.Data
import           Data.Map
import           Data.Outputable
import           Data.Text
import           GHC.Generics
import           Language.Griff.Id

data Ty = TVar Id
        | TArr Ty Ty
        | TPrim TPrim
        | TRecord (Map Text Ty)
        | TVariant (Map Text Ty)
        | TCon Id [Ty]
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Ty

data TPrim = TInt | TChar | TString | TBool
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable TPrim

data Scheme = Forall [Id] Ty
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Scheme
