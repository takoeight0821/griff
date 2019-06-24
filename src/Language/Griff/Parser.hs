{-# LANGUAGE OverloadedStrings #-}
module Language.Griff.Parser (pExp, pDec) where

import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Text                      (Text, pack)
import           Data.Void
import           Language.Griff.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

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

pKeyword :: Text -> Parser ()
pKeyword keyword = void $ lexeme (string keyword <* notFollowedBy alphaNumChar)

opLetter :: Parser Char
opLetter = oneOf ("+-*/%=><:;|&" :: String)

pOperator :: Text -> Parser ()
pOperator op = void $ lexeme (string op <* notFollowedBy opLetter)

reserved :: Parser ()
reserved = void $ choice $ map (try . pKeyword) ["let", "in", "and", "rec", "fn", "case", "of"]

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
  <?> "variable"

pInteger :: Parser (Exp Text)
pInteger = Int <$> getSourcePos <*> integer <?> "integer"

pChar :: Parser (Exp Text)
pChar = Char <$> getSourcePos <*> charLiteral <?> "char"

pString :: Parser (Exp Text)
pString = String <$> getSourcePos <*> stringLiteral <?> "string"

pConstructor :: Parser (Exp Text)
pConstructor = Constructor <$> getSourcePos <*> lexeme upperIdent <?> "constructor"

pSingleExp :: Parser (Exp Text)
pSingleExp = pVariable
  <|> pConstructor
  <|> pInteger
  <|> pChar
  <|> pString
  <|> parens pExp

pApply :: Parser (Exp Text)
pApply = do
  s <- getSourcePos
  f <- pSingleExp
  x <- pSingleExp
  xs <- many pSingleExp
  return $ build s f (x:xs)
  where
    build _ f []       = f
    build s f (x : xs) = build s (Apply s f x) xs

pLambda :: Parser (Exp Text)
pLambda = label "lambda" $ do
  s <- getSourcePos
  _<- pKeyword "fn"
  x <- lexeme lowerIdent
  xs <- many $ lexeme lowerIdent
  _ <- pOperator "->"
  e <- pExp
  return $ Lambda s x $ foldr (Lambda s) e xs

pLet :: Parser (Exp Text)
pLet = label "let" $ do
  s <- getSourcePos
  _ <- pKeyword "let"
  f <- lexeme lowerIdent
  xs <- many (lexeme lowerIdent)
  _ <- pOperator "="
  v <- lexeme pExp
  _ <- pKeyword "in"
  Let s f xs v <$> pExp

pLetRec :: Parser (Exp Text)
pLetRec = label "let rec" $ do
  s <- getSourcePos
  _ <- pKeyword "let"
  _ <- pKeyword "rec"
  f <- pdef
  fs <- many (pKeyword "and" >> pdef)
  _ <- pKeyword "in"
  LetRec s (f:fs) <$> pExp
  where
    pdef = do
      f <- lexeme lowerIdent
      xs <- many $ lexeme lowerIdent
      _ <- pKeyword "="
      e1 <- pExp
      return (f, xs, e1)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser (Exp Text)
pTerm = try pApply
        <|> pSingleExp

pBinOp :: Parser (Exp Text)
pBinOp = makeExprParser pTerm opTable

opTable :: [[Operator Parser (Exp Text)]]
opTable = [
  -- 前置演算子はpSingleOpに対してのみ適用可能にしないといけない
  -- [ prefix "-" $ \s x -> BinOp s Sub (Int s 0) x
  --   , prefix "+" $ \_ x -> x ]
  -- ,
  [ left "*" $ \s l h -> BinOp s Mul l h
  , left "/" $ \s l h -> BinOp s Div l h]
  , [ left "+" $ \s l h -> BinOp s Add l h
    , left "-" $ \s l h -> BinOp s Sub l h]
  ]
  where
    left name f = InfixL (getSourcePos >>= \s -> pOperator name >> return (f s))
    neutral name f = InfixN (getSourcePos >>= \s -> pOperator name >> return (f s))
    right name f = InfixR (getSourcePos >>= \s -> pOperator name >> return (f s))
    prefix name f = Prefix (getSourcePos >>= \s -> pOperator name >> return (f s))
    postfix name f = Postfix (getSourcePos >>= \s -> pOperator name >> return (f s))

pVarPat :: Parser (Pat Text)
pVarPat = label "variable pattern" $ do
  s <- getSourcePos
  x <- lexeme lowerIdent
  return $ VarP s x

pPattern :: Parser (Pat Text)
pPattern = pVarPat

pCase :: Parser (Exp Text)
pCase = label "case expression" $ do
  s <- getSourcePos
  _ <- pKeyword "case"
  e <- pExp
  _ <- pKeyword "of"
  ps <- many clause
  return $ Case s e ps
  where
    clause = do
      _ <- pOperator "|"
      pat <- pPattern
      _ <- pOperator "->"
      e <- pExp
      return (pat, e)

pExp :: Parser (Exp Text)
pExp = try pBinOp
       <|> pTerm
       <|> pLambda
       <|> try pLet -- revert "let"
       <|> pLetRec
       <|> pCase
       <|> pSingleExp

pDec :: Parser (Dec Text)
pDec = pScDef
  -- TODO: TypeDefのパーサ

pScDef :: Parser (Dec Text)
pScDef = label "toplevel function definition" $ do
  s <- getSourcePos
  f <- lexeme lowerIdent
  xs <- many (lexeme lowerIdent)
  _ <- pOperator "="
  v <- lexeme pExp
  return $ ScDef s f xs v
