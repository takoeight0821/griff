{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Griff.Typing.Env where

import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Monoid
import           Data.Outputable
import           GHC.Generics
import           Language.Griff.Id
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Subst
import           Prelude                     hiding (lookup)

newtype Env = Env { types :: Map Id Scheme }
  deriving (Show, Eq, Monoid, Semigroup, Generic)

instance Outputable Env

instance Substitutable Env where
  apply s (Env env) = Env $ fmap (apply s) env
  ftv (Env env) = ftv $ Map.elems env

extend :: Env -> (Id, Scheme) -> Env
extend (Env env) (x, s) = Env $ Map.insert x s env

remove :: Env -> Id -> Env
remove (Env env) x = Env $ Map.delete x env

extends :: Env -> [(Id, Scheme)] -> Env
extends (Env env) xs = Env $ Map.union (Map.fromList xs) env

merge :: Env -> Env -> Env
merge (Env e1) (Env e2) = Env $ Map.union e1 e2

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl merge mempty

singleton :: Id -> Scheme -> Env
singleton name scm = Env $ Map.singleton name scm

keys :: Env -> [Id]
keys (Env env) = Map.keys env

fromList :: [(Id, Scheme)] -> Env
fromList = Env . Map.fromList

toList :: Env -> [(Id, Scheme)]
toList (Env env) = Map.toList env
