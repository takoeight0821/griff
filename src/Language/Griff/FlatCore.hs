{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Griff.FlatCore where

import           GHC.Generics
import           Language.Griff.Core    (Op)
import           Language.Griff.Id
import           Language.Griff.Prelude
import           Text.Show.Pretty       (PrettyVal)

data Atom = Var Id
          | Int Integer
          | Char Char
          | String Text
          | Bool Bool
  deriving (Eq, Show, Generic, PrettyVal)

data Tag = Tuple
         | Cons Int
         | Func Id
  deriving (Eq, Show, Generic, PrettyVal)

data Exp = Atom Atom
         | Node Tag [Atom]
         | Apply Id [Atom]
         | Prim Op [Id]
         | Case Id [(Pat, [Stmt])]
  deriving (Eq, Show, Generic, PrettyVal)

data Stmt = Let Id Exp
          | Pure Exp
  deriving (Eq, Show, Generic, PrettyVal)

data Pat = VarP Id
         | BoolP Bool
         | NodeP Tag [Id]
  deriving (Eq, Show, Generic, PrettyVal)

data ScDef = ScDef
  { _name       :: Id
  , _parameters :: [Id]
  , _body       :: [Stmt]
  } deriving (Eq, Show, Generic, PrettyVal)
