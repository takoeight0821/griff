module Language.Griff.TypeRep where

import Data.Map
import Data.Text
import Language.Griff.Id

data Ty = TVar Id
        | TArr Ty Ty
        | TPrim TPrim
        | TRecord (Map Text Ty)
        | TVariant (Map Text Ty)
  deriving (Eq, Ord, Show)

data TPrim = TInt | TChar | TString
  deriving (Eq, Ord, Show)

data Scheme = Forall [Id] Ty
  deriving (Eq, Ord, Show)
