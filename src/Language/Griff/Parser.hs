{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Language.Griff.Parser (pExp, pDec, pDecs) where

import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Text                      (Text, pack)
import           Data.Void
import           Language.Griff.Syntax
import           Language.Griff.TypeRep
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

-- float :: Parser Double
-- float = lexeme L.float

pKeyword :: Text -> Parser ()
pKeyword keyword = void $ lexeme (string keyword <* notFollowedBy alphaNumChar)

opLetter :: Parser Char
opLetter = oneOf ("+-*/%=><:;|&" :: String)

pOperator :: Text -> Parser ()
pOperator op = void $ lexeme (string op <* notFollowedBy opLetter)

reserved :: Parser ()
reserved = void $ choice $ map (try . pKeyword) ["let", "in", "and", "rec", "fn", "case", "of", "as", "type", "true", "false", "Int", "Bool", "Char", "String"]

lowerIdent :: Parser Text
lowerIdent = lexeme $ do
  notFollowedBy reserved
  pack <$> ((:) <$> lowerChar <*> many alphaNumChar <?> "lower identifier")

upperIdent :: Parser Text
upperIdent = lexeme $ do
  notFollowedBy reserved
  pack <$> ((:) <$> upperChar <*> many alphaNumChar <?> "upper identifier")

pVariable :: Parser (Exp Text)
pVariable =
  Var <$> getSourcePos
  <*> lowerIdent
  <?> "variable"

pInteger :: Parser (Exp Text)
pInteger = Int <$> getSourcePos <*> integer <?> "integer"

pChar :: Parser (Exp Text)
pChar = Char <$> getSourcePos <*> charLiteral <?> "char"

pString :: Parser (Exp Text)
pString = String <$> getSourcePos <*> stringLiteral <?> "string"

pRecord :: Parser (Exp Text)
pRecord = label "record" $ between (symbol "{") (symbol "}") $
  Record <$> getSourcePos <*> field `sepBy` symbol ","
  where
    field = (,) <$> lowerIdent <* pOperator "=" <*> pExp

pProj :: Parser (Exp Text)
pProj = label "proj" $ between (symbol "<") (symbol ">") $
  Proj <$> getSourcePos <*> lowerIdent <* pOperator "=" <*> pExp

pSingleExp :: Parser (Exp Text)
pSingleExp =
  try (Bool <$> getSourcePos <* pKeyword "true" <*> pure True)
  <|> try (Bool <$> getSourcePos <* pKeyword "false" <*> pure False)
  <|> pVariable
  <|> pInteger
  <|> pChar
  <|> pString
  <|> pRecord
  <|> pProj
  <|> parens pExp

pApply :: Parser (Exp Text)
pApply = do
  s <- getSourcePos
  f <- pSingleExp
  x <- pSingleExp
  xs <- many pSingleExp
  return $ foldl (Apply s) f (x:xs)

pLambda :: Parser (Exp Text)
pLambda = label "lambda" $ do
  s <- getSourcePos
  pKeyword "fn"
  x <- lowerIdent
  xs <- many lowerIdent
  pOperator "->"
  e <- pExp
  return $ Lambda s x $ foldr (Lambda s) e xs

pLet :: Parser (Exp Text)
pLet = label "let" $
  Let <$> getSourcePos <* pKeyword "let" <*> lowerIdent <*> many lowerIdent <* pOperator "=" <*> pExp <* pKeyword "in" <*> pExp

pLetRec :: Parser (Exp Text)
pLetRec = label "let rec" $ do
  s <- getSourcePos
  pKeyword "let"
  pKeyword "rec"
  f <- pdef
  fs <- many (pKeyword "and" >> pdef)
  pKeyword "in"
  LetRec s (f:fs) <$> pExp
  where
    pdef = do
      f <- lowerIdent
      xs <- many $ lowerIdent
      pKeyword "="
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
  [ left "*" $ \s l h -> BinOp s Mul l h
  , left "/" $ \s l h -> BinOp s Div l h]
  , [ left "+" $ \s l h -> BinOp s Add l h
    , left "-" $ \s l h -> BinOp s Sub l h]
  ]
  where
    left name f = InfixL (getSourcePos >>= \s -> pOperator name >> return (f s))
    -- neutral name f = InfixN (getSourcePos >>= \s -> pOperator name >> return (f s))
    -- right name f = InfixR (getSourcePos >>= \s -> pOperator name >> return (f s))
    -- prefix name f = Prefix (getSourcePos >>= \s -> pOperator name >> return (f s))
    -- postfix name f = Postfix (getSourcePos >>= \s -> pOperator name >> return (f s))

pVarPat :: Parser (Pat Text)
pVarPat = label "variable pattern" $ do
  s <- getSourcePos
  x <- lowerIdent
  return $ VarP s x

pRecordPat :: Parser (Pat Text)
pRecordPat = label "record pattern" $ do
  s <- getSourcePos
  void $ symbol "{"
  xs <- sepBy field (symbol ",")
  void $ symbol "}"
  return $ RecordP s xs
  where
    field = (,) <$> lowerIdent <* pOperator "=" <*> pPattern

pVariantPat :: Parser (Pat Text)
pVariantPat = label "variant pattern" $ do
  s <- getSourcePos
  void $ symbol "<"
  x <- lowerIdent
  pOperator "="
  p <- pPattern
  void $ symbol ">"
  return $ VariantP s x p

pPattern :: Parser (Pat Text)
pPattern = pVarPat
           <|> pRecordPat
           <|> pVariantPat

pCase :: Parser (Exp Text)
pCase = label "case expression" $ do
  s <- getSourcePos
  pKeyword "case"
  e <- pExp
  pKeyword "of"
  ps <- many clause
  return $ Case s e ps
  where
    clause = do
      pOperator "|"
      pat <- pPattern
      pOperator "->"
      e <- pExp
      return (pat, e)

pAscribe :: Parser (Exp Text)
pAscribe = Ascribe <$> getSourcePos <*> pTerm <* pKeyword "as" <*> pType

pExp :: Parser (Exp Text)
pExp = try pAscribe
       <|>try pBinOp
       <|> pTerm
       <|> pLambda
       <|> try pLet -- revert "let"
       <|> pLetRec
       <|> pCase
       <|> do { s <- getSourcePos
              ; pOperator "-"
              ; BinOp s Sub (Int s 0) <$> pSingleExp }
       <|> do { pOperator "+"
              ; pSingleExp }
       <|> pSingleExp

pDecs :: Parser [Dec Text]
pDecs = many (pDec <* symbol ";")

pDec :: Parser (Dec Text)
pDec = try pScSig
       <|> pScDef
       <|> pTypeAliasDef

pScSig :: Parser (Dec Text)
pScSig = label "toplevel function signature" $
  ScSig <$> getSourcePos <*> lowerIdent <* symbol ":" <*> pType

pScDef :: Parser (Dec Text)
pScDef = label "toplevel function definition" $
  ScDef <$> getSourcePos <*> lowerIdent <*> many lowerIdent <* pOperator "=" <*> pExp

pTypeAliasDef :: Parser (Dec Text)
pTypeAliasDef = label "type alias definition" $
  TypeAliasDef <$> getSourcePos <* pKeyword "type" <*> upperIdent <*> many lowerIdent <* pOperator "=" <*> pType

pType :: Parser (Type Text)
pType = try pTyArr
        <|> try pTyApp
        <|> pSingleType

pTyArr :: Parser (Type Text)
pTyArr =
  TyArr <$> getSourcePos <*> pSingleType <* pOperator "->" <*> pType

pTyApp :: Parser (Type Text)
pTyApp =
  TyApp <$> getSourcePos <*> upperIdent <*> many pSingleType

pSingleType :: Parser (Type Text)
pSingleType =
  try (TyPrim <$> getSourcePos <* pKeyword "Int" <*> pure TInt)
  <|> try (TyPrim <$> getSourcePos <* pKeyword "Char" <*> pure TChar)
  <|> try (TyPrim <$> getSourcePos <* pKeyword "String" <*> pure TString)
  <|> try (TyPrim <$> getSourcePos <* pKeyword "Bool" <*> pure TBool)
  <|> TyVar <$> getSourcePos <*> lowerIdent
  <|> TyApp <$> getSourcePos <*> upperIdent <*> pure []
  <|> pTyRecord
  <|> pTyVariant
  <|> between (symbol "(") (symbol ")") pType
  where
    pTyRecord = TyRecord <$> getSourcePos <*> between (symbol "{") (symbol "}") (field `sepBy` symbol ",")
    pTyVariant = TyVariant <$> getSourcePos <*> between (symbol "<") (symbol ">") (field `sepBy` symbol "|")
    field = (,) <$> lowerIdent <* pOperator ":" <*> pType
