{-# LANGUAGE OverloadedStrings #-}
module Language.Griff.Parser (pExp) where

import Language.Griff.IR.Syntax
import           Data.Text                  (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

pInt :: Parser (Exp Text)
pInt = Int <$> getSourcePos <*> integer

pExp :: Parser (Exp Text)
pExp = pInt
