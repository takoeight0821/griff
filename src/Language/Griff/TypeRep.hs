module Language.Griff.TypeRep where

import Data.Text
import Language.Griff.Id

data TVar = Meta Int
          | Gen Id
          | Field Text TypeRep
  deriving (Eq, Ord, Show)

data TypeRep = TApp TypeRep TypeRep
             | TVar TVar
             | TCon Id
             | TRecord [(Text, TypeRep)]
             | TVariant [(Text, TypeRep)]
  deriving (Eq, Ord, Show)

data Scheme = Forall [Id] TypeRep
  deriving (Eq, Ord, Show)
