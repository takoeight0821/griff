{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Griff.Pretty
  ( module Text.PrettyPrint.HughesPJClass,
    (<+>),
    pShow,
    errorDoc,
    errorOn,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Language.Griff.Prelude
import Text.Pretty.Simple (pShow)
import Text.PrettyPrint.HughesPJClass (Doc, Pretty (..))
import qualified Text.PrettyPrint.HughesPJClass as P
import qualified Prelude
import Text.Megaparsec.Pos (SourcePos)

-- change operator precedence
infixl 9 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (P.<+>)

instance Pretty T.Text where
  pPrint = P.text . T.unpack

instance Pretty TL.Text where
  pPrint = P.text . TL.unpack

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ P.render x


errorOn :: HasCallStack => SourcePos -> Doc -> a
errorOn pos x = errorDoc $ "error on" <+> pPrint pos <> ":" P.$+$ P.nest 2 x