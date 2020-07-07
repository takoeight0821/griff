{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Parser where

import Control.Monad.Combinators.Expr
import qualified Data.Text as T
import Data.Void
import Language.Griff.Id
import Language.Griff.Prelude hiding (many, some)
import Language.Griff.Syntax
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

identLetter :: Parser Char
identLetter = alphaNumChar <|> char '_'

opLetter :: Parser Char
opLetter = oneOf ("+-*/%=><:;|&" :: String)

pKeyword :: Text -> Parser ()
pKeyword keyword = void $ lexeme (string keyword <* notFollowedBy identLetter)

reserved :: Parser ()
reserved = void $ choice $ map (try . pKeyword) []

lowerIdent :: Parser Name
lowerIdent = label "lower identifier" $ lexeme $ do
  notFollowedBy reserved
  Name . T.pack <$> ((:) <$> (lowerChar <|> char '_') <*> many identLetter)

upperIdent :: Parser Name
upperIdent = label "upper identifier" $ lexeme $ do
  notFollowedBy reserved
  Name . T.pack <$> ((:) <$> upperChar <*> many identLetter)

operator :: Parser Name
operator = label "operator" $ lexeme $ do
  Name . T.pack <$> some opLetter

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

pVariable :: Parser (Exp (Griff 'Parse))
pVariable =
  label "variable" $
    Var <$> getSourcePos <*> lowerIdent

pConstructor :: Parser (Exp (Griff 'Parse))
pConstructor =
  label "constructor" $
    Con <$> getSourcePos <*> upperIdent

pFun :: Parser (Exp (Griff 'Parse))
pFun =
  label "function literal" $ between (symbol "{") (symbol "}") $
    Fn <$> getSourcePos
      <*> ( Clause <$> getSourcePos
              <*> (try (some pPat <* symbol "->") <|> pure [])
              <*> pExp
          )
        `sepBy` symbol "|"
  where
    pSinglePat =
      VarP <$> getSourcePos <*> lowerIdent
        <|> ConP <$> getSourcePos <*> upperIdent <*> pure []
        <|> between (symbol "(") (symbol ")") pPat
    pPat =
      label "pattern" $
        ConP <$> getSourcePos <*> upperIdent <*> some pSinglePat
          <|> pSinglePat

pSingleExp :: Parser (Exp (Griff 'Parse))
pSingleExp =
  Unboxed <$> getSourcePos <*> pUnboxed
    <|> pVariable
    <|> pConstructor
    <|> pFun
    <|> between (symbol "(") (symbol ")") pExp

pApply :: Parser (Exp (Griff 'Parse))
pApply = do
  s <- getSourcePos
  f <- pSingleExp
  xs <- some pSingleExp
  pure $ foldl (Apply s) f xs

pTerm :: Parser (Exp (Griff 'Parse))
pTerm = try pApply <|> pSingleExp

pOpApp :: Parser (Exp (Griff 'Parse))
pOpApp = makeExprParser pTerm opTable
  where
    opTable =
      [ [ InfixL $ do
            s <- getSourcePos
            op <- Var <$> getSourcePos <*> operator
            pure $ \l r -> OpApp s op l r
        ]
      ]

pExp :: Parser (Exp (Griff 'Parse))
pExp =
  try pOpApp
    <|> pTerm
