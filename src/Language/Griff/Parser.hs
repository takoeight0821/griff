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
identLetter = alphaNumChar <|> oneOf ("_#" :: String)

opLetter :: Parser Char
opLetter = oneOf ("+-*/%=><:;|&" :: String)

pKeyword :: Text -> Parser ()
pKeyword keyword = void $ lexeme (string keyword <* notFollowedBy identLetter)

pOperator :: Text -> Parser ()
pOperator op = void $ lexeme (string op <* notFollowedBy opLetter)

reserved :: Parser ()
reserved = void $ choice $ map (try . pKeyword) ["data", "infixl", "infixr", "infix", "forign", "import"]

reservedOp :: Parser ()
reservedOp = void $ choice $ map (try . pOperator) ["=", "::", "|", "->", ";"]

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
  notFollowedBy reservedOp
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
              <*> (try (some pPat <* pOperator "->") <|> pure [])
              <*> pExp
          )
        `sepBy` pOperator "|"

pSinglePat :: Parser (Pat (Griff 'Parse))
pSinglePat =
  VarP <$> getSourcePos <*> lowerIdent
    <|> ConP <$> getSourcePos <*> upperIdent <*> pure []
    <|> between (symbol "(") (symbol ")") pPat

pPat :: Parser (Pat (Griff 'Parse))
pPat =
  label "pattern" $
    try (ConP <$> getSourcePos <*> upperIdent <*> some pSinglePat)
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

pTyVar :: Parser (Type (Griff 'Parse))
pTyVar = label "type variable" $ TyVar <$> getSourcePos <*> lowerIdent

pTyCon :: Parser (Type (Griff 'Parse))
pTyCon = label "type constructor" $
  TyCon <$> getSourcePos <*> upperIdent

pSingleType :: Parser (Type (Griff 'Parse))
pSingleType = pTyVar <|> pTyCon <|> between (symbol "(") (symbol ")") pType

pTyApp :: Parser (Type (Griff 'Parse))
pTyApp = TyApp <$> getSourcePos <*> pSingleType <*> some pSingleType

pTyTerm :: Parser (Type (Griff 'Parse))
pTyTerm = try pTyApp <|> pSingleType

pTyArr :: Parser (Type (Griff 'Parse))
pTyArr = makeExprParser pTyTerm opTable
  where
    opTable =
      [ [ InfixR $ do
            s <- getSourcePos
            void $ pOperator "->"
            pure $ \l r -> TyArr s l r
        ]
      ]

pType :: Parser (Type (Griff 'Parse))
pType = try pTyArr <|> pTyTerm

pScDef :: Parser (Decl (Griff 'Parse))
pScDef =
  label "toplevel function definition" $
    ScDef <$> getSourcePos <*> lowerIdent <*> many lowerIdent <* pOperator "=" <*> pExp

pScSig :: Parser (Decl (Griff 'Parse))
pScSig =
  label "toplevel function signature" $
    ScSig <$> getSourcePos <*> lowerIdent <* pOperator "::" <*> pType

pDataDef :: Parser (Decl (Griff 'Parse))
pDataDef = label "toplevel type definition" $ do
  s <- getSourcePos
  void $ pKeyword "data"
  d <- upperIdent
  xs <- many lowerIdent
  void $ pOperator "="
  ts <- pConDef `sepBy` pOperator "|"
  pure $ DataDef s d xs ts
  where
    pConDef = (,) <$> upperIdent <*> many pType

pInfix :: Parser (Decl (Griff 'Parse))
pInfix = label "infix declaration" $ do
  s <- getSourcePos
  a <- try (pKeyword "infixl" *> pure LeftA) <|> try (pKeyword "infixr" *> pure RightA) <|> (pKeyword "infix" *> pure NeutralA)
  i <- lexeme L.decimal
  x <- operator
  pure $ Infix s a i x

pForign :: Parser (Decl (Griff 'Parse))
pForign = label "forign import" $ do
  s <- getSourcePos
  pKeyword "forign"
  pKeyword "import"
  x <- lowerIdent
  pOperator "::"
  t <- pType
  pure $ Forign s x t
   
pDecl :: Parser (Decl (Griff 'Parse))
pDecl = pDataDef <|> pInfix <|> pForign <|> try pScSig <|> pScDef

pTopLevel :: Parser [Decl (Griff 'Parse)]
pTopLevel = pDecl `sepBy` pOperator ";" <* eof