{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Language.Griff.Parse where

import           Capability.Reader
import           Control.Applicative
import           Data.SCargot
import           Data.SCargot.Atom
import           Data.SCargot.Comments
import           Data.SCargot.Common
import           Data.SCargot.Repr
import           Data.SCargot.Repr.WellFormed
import           Data.Text                    (Text, pack)
import           Language.Griff.Monad
import           Language.Griff.SrcSpan
import           Language.Griff.Syntax
import           Text.Parsec                  hiding ((<|>))

import           Text.Parsec.Text             (Parser)

data Atom = Symbol Text
          | IntL Integer
          | Operator Text
  deriving (Eq, Ord, Show)

atomParser :: Parser (Located Atom)
atomParser = located $
  atom Symbol parseXIDIdentGeneral
  <|> atom Operator (pack <$> string ":")
  <|> atom IntL signedDecNumber

parser :: SExprParser (Located Atom) (WellFormedSExpr (Located Atom))
parser = setCarrier toWellFormed $ withLispComments $ mkParser atomParser

parse :: Text -> Either String [WellFormedSExpr (Located Atom)]
parse = decode parser

toSrcSpan :: Monad m => Location -> GriffT m SrcSpan
toSrcSpan (Span start end) = do
  fileName <- ask @"fileName"
  return $ SrcSpan fileName (sourceLine start) (sourceColumn start) (sourceLine end) (sourceColumn end)

toSyntax :: Monad m => [WellFormedSExpr (Located Atom)] -> GriffT m [Dec Text]
toSyntax (L [A (At loc (Operator ":")), A (At _ (Symbol f)), e] : xs) = undefined
