{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Griff.Driver (compileFromPath) where

import           Control.Effect
import           Control.Effect.Error
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Text                   (Text)
import qualified Data.Text.IO                as T
import qualified Language.Griff.Desugar      as Desugar
import qualified Language.Griff.KNormal      as KNormal
import           Language.Griff.Parser
import           Language.Griff.Prelude
import           Language.Griff.Rename
import           Language.Griff.Syntax
import           Language.Griff.Typing.Infer
import           Language.Griff.Typing.Monad
import           Language.Griff.Uniq
import           Text.Megaparsec
import           Text.Show.Pretty

compileFromPath :: (MonadFail m, MonadIO m) => FilePath -> m ()
compileFromPath path = runM $ do
  result <- runError $ runUniq $ do
    src <- liftIO $ T.readFile path
    ast <- parseSource src
    renamed <- rename ast
    r <- runInfer mempty $ do
      conMap <- infer renamed
      desugared <- Desugar.desugar renamed conMap
      knormalized <- KNormal.convert desugared
      liftIO $ dumpIO knormalized
    case r of
      Right _  -> pure ()
      Left err -> throwError $ dumpStr err
  case result of
    Right () -> pure ()
    Left err -> error err

parseSource :: (Member (Error String) sig, Carrier sig f) => Text -> f [Dec Text]
parseSource src = do
  let ast = parse (pDecs <* eof) "<stdin>" src
  case ast of
    Right ast' -> pure ast'
    Left err   -> throwError $ errorBundlePretty err
