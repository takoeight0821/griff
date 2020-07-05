{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- trees that growをベースに設計
module Language.Griff.Syntax where

import Data.Int (Int32, Int64)
import Language.Griff.Prelude
import Language.Griff.Id
import Data.Kind (Constraint, Type)

-- Unboxed literal

data Unboxed = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String String
  deriving stock (Show, Eq, Ord)

-- Expression

data Exp x
  = Var (XVar x) (XId x) -- variable and constructor
  | Unboxed (XUnboxed x) Unboxed
  | Apply (XApply x) (Exp x) (Exp x)
  | Fn (XFn x) [Clause x]

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x) => Eq (Exp x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x) => Show (Exp x)

-- Clause

data Clause x = Clause (XClause x) [Pat x] (Exp x)

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x) => Show (Clause x)

-- Pattern

data Pat x
  = VarP (XVarP x)
  | ConP (XConP x) [Pat x]

deriving stock instance ForallPatX Eq x => Eq (Pat x)

deriving stock instance ForallPatX Show x => Show (Pat x)

-- Extension

-- Exp Extensions
type family XVar x

type family XId x

type family XUnboxed x

type family XApply x

type family XFn x

type ForallExpX (c :: Type -> Constraint) x = (c (XVar x), c (XId x), c (XUnboxed x), c (XApply x), c (XFn x))

data GriffPhase = Parse | Rename | TypeCheck

-- Clause Extensions
type family XClause x

type ForallClauseX (c :: Type -> Constraint) x = c (XClause x)

-- Pat Extensions
type family XVarP x

type family XConP x

type ForallPatX (c :: Type -> Constraint) x = (c (XVarP x), c (XConP x))

-- Phase and type instance
data Griff (p :: GriffPhase)

data NoExt = NoExt
  deriving stock (Eq, Show)

type instance XVar (Griff _) = NoExt

type instance XId (Griff p) = GriffId p

type family GriffId (p :: GriffPhase) where
  GriffId 'Parse = String
  GriffId 'Rename = Id ()

type instance XUnboxed (Griff _) = NoExt

type instance XApply (Griff _) = NoExt

type instance XFn (Griff _) = NoExt

type instance XClause (Griff _) = NoExt

type instance XVarP (Griff p) = GriffId p

type instance XConP (Griff p) = GriffId p