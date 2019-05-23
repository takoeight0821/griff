{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyDataDeriving  #-}
{-# LANGUAGE KindSignatures     #-}
module Language.Griff.Syntax where

import           Control.Lens.Plated
import           Data.Data
import           Data.Outputable
import           GHC.Generics
import           Language.Griff.SrcSpan

data Exp a = Var SrcSpan a
           | Int SrcSpan Integer
           | Bool SrcSpan Bool
           | Char SrcSpan Char
           | String SrcSpan String
           | Constructor SrcSpan a
           | Apply SrcSpan (Exp a) (Exp a)
           | Lambda SrcSpan (Pat a) (Exp a)
           | Let SrcSpan (Pat a) (Exp a) (Exp a)
           | LetRec SrcSpan [(Pat a, Exp a)] (Exp a)
           -- -- | Parens SrcSpan (Exp a)
           | BinOp SrcSpan Op (Exp a) (Exp a)
           | Case SrcSpan (Exp a) [(Pat a, Exp a)] (Exp a)
           | If SrcSpan (Exp a) (Exp a) (Exp a)
           | Error SrcSpan String
  deriving (Eq, Ord, Show, Generic, Data)

-- Caseの最後の引数は、どのパターンにもマッチしなかったときに実行される(パース時は大抵Error)

instance Outputable a => Outputable (Exp a)
instance Data a => Plated (Exp a)

instance HasSrcSpan (Exp a) where
  srcSpan (Var ss _)         = ss
  srcSpan (Int ss _)         = ss
  srcSpan (Bool ss _)        = ss
  srcSpan (Char ss _)        = ss
  srcSpan (String ss _)      = ss
  srcSpan (Constructor ss _) = ss
  srcSpan (Apply ss _ _)     = ss
  srcSpan (Lambda ss _ _)    = ss
  srcSpan (Let ss _ _ _)     = ss
  srcSpan (LetRec ss _ _)    = ss
  srcSpan (BinOp ss _ _ _)   = ss
  srcSpan (Case ss _ _ _)    = ss
  srcSpan (If ss _ _ _)      = ss
  srcSpan (Error ss _)       = ss

data Op = Add | Sub | Mul | Div | Mod | FAdd | FSub | FMul | FDiv | Eq | Neq | Lt | Le | Gt | Ge | And | Or
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable Op

data Pat a = VarP SrcSpan a
           | IntP SrcSpan Integer
           | BoolP SrcSpan Bool
           | CharP SrcSpan Char
           | StringP SrcSpan String
           | ConstructorP SrcSpan a [Pat a]
  deriving (Eq, Ord, Show, Generic, Data, Foldable)

instance Outputable a => Outputable (Pat a)

data Dec a = ScAnn SrcSpan a (Type a)
           | ScDef SrcSpan a (Exp a)
           | TypeDef SrcSpan a [a] [Type a]
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable a => Outputable (Dec a)

data Type a = TyApp SrcSpan (Type a) (Type a)
            | TyCon SrcSpan a
            | TyVar SrcSpan a
  deriving (Eq, Ord, Show, Generic, Data)

instance Outputable a => Outputable (Type a)
