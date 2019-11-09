{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Griff.NewParser
  ( parse
  )
where

import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import           Data.String                    (fromString)
import           Data.Void
import           Language.Griff.Prelude
import           Language.Griff.Syntax
import           Language.Griff.TypeRep
import           Text.Megaparsec                hiding (parse)
import qualified Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Debug.Trace
import           Text.Megaparsec.Debug

type Parser = Parsec Void Text

parse :: String -> Text -> Either (ParseErrorBundle Text Void) [Dec Text]
parse = Text.Megaparsec.parse (parseDecs <* eof)

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "{-" "-}"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

scn :: Parser ()
scn = L.space space1 lineComment blockComment

parens :: Text -> Text -> Parser a -> Parser a
parens left right inter =
  between (symbol left <* try scn) (symbol right) (inter <* try scn)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

consumeKeyword :: Text -> Parser ()
consumeKeyword keyword =
  void $ lexeme (string keyword <* notFollowedBy alphaNumChar)

consumeOperator :: Text -> Parser ()
consumeOperator op = void $ lexeme $ string op <* notFollowedBy
  (oneOf opLetter)
  where opLetter = "+-*/%=><:;|&" :: String

reserved :: Parser ()
reserved = void $ choice $ map
  (try . consumeKeyword)
  [ "let"
  , "in"
  , "fn"
  , "case"
  , "of"
  , "as"
  , "type"
  , "True"
  , "False"
  , "Int"
  , "Bool"
  , "Char"
  , "String"
  , "if"
  , "then"
  , "else"
  ]

lowerIdent :: Parser Text
lowerIdent = label "lower identifier" $ lexeme $ fromString <$> do
  notFollowedBy reserved
  (:) <$> (lowerChar <|> char '_') <*> many (alphaNumChar <|> char '_')

upperIdent :: Parser Text
upperIdent = label "upper identifier" $ lexeme $ fromString <$> do
  notFollowedBy reserved
  (:) <$> upperChar <*> many alphaNumChar

parseVar :: Parser (Exp Text)
parseVar = label "variable" $ Var <$> getSourcePos <*> lowerIdent

parseInt :: Parser (Exp Text)
parseInt = label "integer" $ Int <$> getSourcePos <*> integer
  where integer = lexeme L.decimal

parseChar :: Parser (Exp Text)
parseChar =
  label "char"
    $   lexeme
    $   Char
    <$> getSourcePos
    <*> between (char '\'') (char '\'') L.charLiteral

parseString :: Parser (Exp Text)
parseString =
  label "string"
    $   lexeme
    $   String
    <$> getSourcePos
    <*> (fromString <$> (char '\"' >> manyTill L.charLiteral (char '\"')))

parseRecord :: Parser (Exp Text)
parseRecord =
  label "record"
    $       parens "{" "}"
    $       Record
    <$>     getSourcePos
    <*>     parseField
    `sepBy` L.symbol scn ","

parseProj :: Parser (Exp Text)
parseProj = label "proj" $ parens "<" ">" $ do
  pos        <- getSourcePos
  (key, val) <- parseField
  pure $ Proj pos key val

parseField :: Parser (Text, Exp Text)
parseField = do
  key <- lowerIdent <* try scn
  consumeOperator "=" <* try scn
  val <- parseExp <* try scn
  return (key, val)

parseTrue :: Parser (Exp Text)
parseTrue = Bool <$> getSourcePos <* consumeKeyword "True" <*> pure True

parseFalse :: Parser (Exp Text)
parseFalse = Bool <$> getSourcePos <* consumeKeyword "False" <*> pure False

parseSingleExp :: Parser (Exp Text)
parseSingleExp =
  try parseTrue
    <|> try parseFalse
    <|> parseVar
    <|> parseInt
    <|> parseChar
    <|> parseString
    <|> parseRecord
    <|> parseProj
    <|> parens "(" ")" parseExp

parseApply :: Parser (Exp Text)
parseApply = L.lineFold scn $ \sc' -> do
  s  <- getSourcePos
  f  <- parseSingleExp
  try sc'
  x  <-  parseSingleExp
  xs <- many (try sc' >> parseSingleExp)
  pure $ foldl (Apply s) f (x : xs)

parseLambda :: Parser (Exp Text)
parseLambda = label "lambda" $ L.lineFold scn $ \sc' -> do
  s <- getSourcePos
  consumeKeyword "fn"
  x  <- lowerIdent
  xs <- many lowerIdent
  try sc'
  consumeOperator "=>"
  try sc'
  e <- parseExp
  pure $ foldr (Lambda s) e (x : xs)

parseLet :: Parser (Exp Text)
parseLet = label "let" $ do
  s <- getSourcePos
  consumeKeyword "let"
  fs <- L.lineFold scn $ \sc' -> parseDef `sepBy1` try sc'
  L.lineFold scn $ \sc' -> do
    consumeKeyword "in"
    try sc'
    LetRec s fs <$> parseExp
 where
  parseDef = L.lineFold scn $ \sc' -> do
    f  <- lowerIdent
    xs <- many lowerIdent
    try sc'
    consumeOperator "="
    try sc'
    body <- parseExp
    pure (f, xs, body)

parseTerm :: Parser (Exp Text)
parseTerm = try parseApply <|> parseSingleExp

parseBinOp :: Parser (Exp Text)
parseBinOp = L.lineFold scn $ \sc' -> makeExprParser parseTerm (opTable sc')
 where
  opTable sc' =
    [ [ neutral sc' "==" $ \s l h -> BinOp s Eq l h
      , neutral sc' "/=" $ \s l h -> BinOp s Neq l h
      ]
    , [ left sc' "*" $ \s l h -> BinOp s Mul l h
      , left sc' "/" $ \s l h -> BinOp s Div l h
      ]
    , [ left sc' "+" $ \s l h -> BinOp s Add l h
      , left sc' "-" $ \s l h -> BinOp s Sub l h
      ]
    ]
  left sc' name f = InfixL
    (getSourcePos >>= \s -> try sc' >> consumeOperator name >> return (f s))
  neutral sc' name f = InfixN
    (getSourcePos >>= \s -> try sc' >> consumeOperator name >> return (f s))

parseIf :: Parser (Exp Text)
parseIf = label "if" $ do
  s     <- getSourcePos
  level <- L.indentLevel
  c     <- L.lineFold scn $ \sc' -> do
    consumeKeyword "if"
    try sc'
    parseExp
  void $ try (L.indentGuard scn EQ level) <|> L.indentGuard scn GT level
  t <- L.lineFold scn $ \sc' -> do
    consumeKeyword "then"
    try sc'
    parseExp
  void $ try (L.indentGuard scn EQ level) <|> L.indentGuard scn GT level
  f <- L.lineFold scn $ \sc' -> do
    consumeKeyword "else"
    try sc'
    parseExp
  pure $ If s c t f

parseCase :: Parser (Exp Text)
parseCase = label "case" $ do
  s <- getSourcePos
  consumeKeyword "case"
  e <- parseExp
  consumeKeyword "of"
  ps <- do
    pos <- L.indentGuard scn GT (sourceColumn s)
    p <- clause
    ps <- many $ do
      void $ L.indentGuard scn EQ pos
      clause
    pure (p:ps)
  pure $ Case s e ps
 where
  clause = do
    pat <- parsePat
    consumeOperator "=>"
    val <- parseExp
    return (pat, val)

parsePat :: Parser (Pat Text)
parsePat = parseVarP <|> parseRecordP <|> parseVariantP

parseVarP :: Parser (Pat Text)
parseVarP = label "variable pattern" $ VarP <$> getSourcePos <*> lowerIdent

parseRecordP :: Parser (Pat Text)
parseRecordP = label "record pattern" $ RecordP <$> getSourcePos <*> parens
  "{"
  "}"
  (parseFieldP `sepBy` L.symbol scn ",")

parseVariantP :: Parser (Pat Text)
parseVariantP = label "variant pattern" $ do
  s          <- getSourcePos
  (key, pat) <- parens "<" ">" parseFieldP <* try scn
  consumeKeyword "as" <* try scn
  VariantP s key pat <$> parseType

parseFieldP :: Parser (Text, Pat Text)
parseFieldP =
  (,)
    <$> lowerIdent
    <*  try scn
    <*  consumeOperator "="
    <*  try scn
    <*> parsePat
    <*  try scn

parseExp :: Parser (Exp Text)
parseExp =
  try parseBinOp
    <|> try parseTerm
    <|> parseLambda
    <|> parseLet
    <|> parseIf
    <|> parseCase

parseType :: Parser (Type Text)
parseType =
  try
      (L.lineFold scn $ \sc' ->
        TyArr
          <$> getSourcePos
          <*> parseSingleType
          <*  try sc'
          <*  consumeOperator "->"
          <*  try sc'
          <*> parseType
      )
    <|> try parseTyApp
    <|> parseSingleType

parseTyApp :: Parser (Type Text)
parseTyApp = L.lineFold scn $ \sc' -> do
  s   <- getSourcePos
  con <- upperIdent
  try sc'
  TyApp s con <$> many (parseSingleType <* try sc')

parseSingleType :: Parser (Type Text)
parseSingleType =
  try (parseTyPrim "Int" TInt)
    <|> try (parseTyPrim "Char" TChar)
    <|> try (parseTyPrim "String" TString)
    <|> try (parseTyPrim "Bool" TBool)
    <|> parseTyApp
    <|> TyVar
    <$> getSourcePos
    <*> lowerIdent
    <|> parseTyRecord
    <|> parseTyVariant
    <|> parens "(" ")" parseType
 where
  parseTyPrim name tag =
    TyPrim <$> getSourcePos <* consumeKeyword name <*> pure tag
  parseTyRecord = TyRecord <$> getSourcePos <*> parens
    "{"
    "}"
    (field `sepBy` L.symbol scn ",")
  parseTyVariant = TyVariant <$> getSourcePos <*> parens
    "<"
    ">"
    (field `sepBy` L.symbol scn "|")
  field =
    (,)
      <$> lowerIdent
      <*  try scn
      <*  consumeOperator ":"
      <*  try scn
      <*> parseType

parseDecs :: Parser [Dec Text]
parseDecs = many parseDec

parseDec :: Parser (Dec Text)
parseDec =
  L.nonIndented scn $ try parseScSig <|> parseScDef <|> parseTypeAliasDef

parseScSig :: Parser (Dec Text)
parseScSig = label "function signature" $ L.lineFold scn $ \sc' -> do
  s <- getSourcePos
  f <- lowerIdent <* try sc'
  consumeOperator ":" <* try sc'
  ScSig s f <$> parseType

parseScDef :: Parser (Dec Text)
parseScDef = label "function definition" $ do
  s     <- getSourcePos
  f     <- lowerIdent
  xs    <- many lowerIdent
  consumeOperator "="
  void $ L.indentGuard scn GT pos1
  ScDef s f xs <$> parseExp

parseTypeAliasDef :: Parser (Dec Text)
parseTypeAliasDef = label "type alias definition" $ L.lineFold scn $ \sc' -> do
  s <- getSourcePos
  consumeKeyword "type"
  con <- upperIdent
  xs  <- many lowerIdent
  try sc'
  consumeOperator "=" <* try sc'
  TypeAliasDef s con xs <$> parseType
