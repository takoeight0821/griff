{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Parser where

import Data.Text (pack)
import Data.Void
import Language.Griff.Id
import Language.Griff.Prelude
import Language.Griff.Syntax
import qualified Text.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- conbinators

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- parser

pUnboxed :: Parser Unboxed
pUnboxed =
  label "unboxed literal" $
    Double <$> try (lexeme $ L.float <* char '#')
      <|> Float <$> try (lexeme $ L.float <* string' "F#")
      <|> Int32 <$> try (lexeme $ L.decimal <* char '#')
      <|> Int64 <$> try (lexeme $ L.decimal <* string' "L#")
      <|> Char <$> lexeme (between (char '\'') (char '\'') L.charLiteral <* char '#')
      <|> String <$> lexeme (char '"' *> manyTill L.charLiteral (char '"') <* char '#')

pExp = undefined