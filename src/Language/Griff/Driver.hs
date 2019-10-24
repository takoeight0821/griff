{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
module Language.Griff.Driver (compileFromPath) where

import           Control.Effect
import           Control.Effect.Error
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Outputable
import           Data.Text                   (Text)
import qualified Data.Text.IO                as T
import qualified Language.Griff.Core         as Core
import           Language.Griff.Desugar
import           Language.Griff.Id
import qualified Language.Griff.KNormal      as KNormal
import           Language.Griff.Parser
import           Language.Griff.Prelude
import           Language.Griff.Rename
import           Language.Griff.Syntax
import           Language.Griff.Typing.Infer
import           Language.Griff.Typing.Monad
import           Language.Griff.Uniq
import           Text.Megaparsec

compileFromPath :: (MonadFail m, MonadIO m) => FilePath -> m ()
compileFromPath path = runM $ do
  result <- runError $ runUniq $ do
    src <- liftIO $ T.readFile path
    ast <- parseSource src
    renamed <- rename ast
    (env, conMap) <- typeCheck renamed
    let desugared = desugar renamed env conMap
    knormalized <- knormalize desugared env
    liftIO $ print $ ppr knormalized
  case result of
    Right () -> pure ()
    Left err -> error err

parseSource :: (Member (Error String) sig, Carrier sig f) => Text -> f [Dec Text]
parseSource src = do
  let ast = parse (pDecs <* eof) "<stdin>" src
  case ast of
    Right ast' -> pure ast'
    Left err   -> throwError $ errorBundlePretty err

typeCheck :: (Carrier sig m, MonadFail m, Effect sig, Member (Error String) sig, Member Fresh sig) => [Dec Id] -> m (Env, ConMap)
typeCheck ast = do
  typeEnv <- runInfer mempty (infer ast)
  case typeEnv of
    Right (env, conMap) -> pure (env, conMap)
    Left err            -> throwError $ show $ ppr err

knormalize :: (Carrier sig m, MonadFail m, Effect sig, Member (Error String) sig, Member Fresh sig) => Core.Toplevel -> Env -> m (Env, [(Id, Core.Exp)])
knormalize desugared env = do
  kn <- runInfer env $ mapM (secondM KNormal.convert) (Core._scDef desugared)
  case kn of
    Right e -> pure e
    Left e  -> throwError $ show $ ppr e
