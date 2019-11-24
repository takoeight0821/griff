{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Griff.XCore where

import           Data.Kind
import           GHC.Generics
import           Language.Griff.Prelude

data ExpX x a = VarX (XVar x a) a
              | LitX (XLit x a) Lit
              | RecordX (XRecord x a) [(Text, ExpX x a)]
              | ProjX (XProj x a) Text (ExpX x a)
              | ApplyX (XApply x a) (ExpX x a) (ExpX x a)
              | LambdaX (XLambda x a) a (ExpX x a)
              | LetX (XLet x a) a (ExpX x a) (ExpX x a)
              | PrimX (XPrim x a) [ExpX x a]
              | CaseX (XCase x a) [(XPat x a, ExpX x a)]
              | ExpX (XExp x a)

type family XVar x a
type family XLit x a
type family XRecord x a
type family XProj x a
type family XApply x a
type family XLambda x a
type family XLet x a
type family XPrim x a
type family XCase x a
type family XPat x a
type family XExp x a

data Lit = Int Integer | Char Char | String Text | Bool Bool
  deriving (Eq, Show, Generic)

type ForallX (c :: Type -> Constraint) x a = (c (XVar x a), c (XLit x a), c (XRecord x a), c (XProj x a), c (XApply x a), c (XLambda x a), c (XLet x a), c (XPrim x a), c (XCase x a), c (XPat x a))
