{-# LANGUAGE OverloadedStrings #-}
module Language.Griff.Parser where

import           Control.Monad
import           Data.Text                  (Text, pack)
import           Data.Void
import           Language.Griff.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

reserved :: Parser ()
reserved = void $ choice $ map (try . pKeyword) ["let", "in", "fn", "->", "="]

lowerIdent :: Parser Text
lowerIdent = do
  notFollowedBy reserved
  pack <$> ((:) <$> lowerChar <*> many alphaNumChar <?> "lower identifier")

upperIdent :: Parser Text
upperIdent = do
  notFollowedBy reserved
  pack <$> ((:) <$> upperChar <*> many alphaNumChar <?> "upper identifier")

pVariable :: Parser (Exp Text)
pVariable =
  Var <$> getSourcePos
  <*> lexeme lowerIdent

pInteger :: Parser (Exp Text)
pInteger = Int <$> getSourcePos <*> signedInteger

pChar :: Parser (Exp Text)
pChar = Char <$> getSourcePos <*> charLiteral

pString :: Parser (Exp Text)
pString = String <$> getSourcePos <*> stringLiteral

pConstructor :: Parser (Exp Text)
pConstructor = Constructor <$> getSourcePos <*> lexeme upperIdent

pApply :: Parser (Exp Text)
pApply = do
  s <- getSourcePos
  f <- pSingleExp
  xs <- many pSingleExp
  return $ build s f xs
  where
    build _ f [] = f
    build s f (x : xs) = build s (Apply s f x) xs

pLambda :: Parser (Exp Text)
pLambda = do
  s <- getSourcePos
  _<- pKeyword "fn"
  x <- lexeme lowerIdent
  xs <- many $ lexeme lowerIdent
  _ <- pKeyword "->"
  e <- pExp
  return $ Lambda s x $ foldr (Lambda s) e xs

pLet :: Parser (Exp Text)
pLet = do
  s <- getSourcePos
  _ <- pKeyword "let"
  x <- lexeme lowerIdent
  _ <- pKeyword "="
  v <- lexeme pExp
  _ <- pKeyword "in"
  e <- lexeme pExp
  return $ Let s x v e

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pSingleExp :: Parser (Exp Text)
pSingleExp = pVariable
  <|> pInteger
  <|> pChar
  <|> pString
  <|> parens pExp

pExp :: Parser (Exp Text)
pExp = try pApply
       <|> pLambda
       <|> try pLet
       <|> pSingleExp
