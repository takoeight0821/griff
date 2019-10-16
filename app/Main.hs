{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Effect
import           Control.Monad.IO.Class
import           Data.Outputable
import           Data.String
import           Language.Griff.Core
import           Language.Griff.Desugar
import qualified Language.Griff.KNormal      as KNormal
import           Language.Griff.Parser
import           Language.Griff.Rename
import           Language.Griff.Typing.Infer
import           Language.Griff.Typing.Monad
import           Language.Griff.Uniq
import           Text.Megaparsec

main :: IO ()
main = do
  src <- getContents
  let ast = parse (pDecs <* eof) "<stdin>" (fromString src)
  case ast of
    Right ast -> runM $ runUniq $ do
      liftIO $ putStrLn "=== Parsed ==="
      liftIO $ print $ ppr ast
      ast' <- rename ast
      typeEnv <- runInfer mempty (infer ast')
      liftIO $ putStrLn "=== Type checked ==="
      liftIO $ print $ ppr ast'
      liftIO $ print $ ppr typeEnv
      case typeEnv of
        Right (env, conMap) -> do
          let desugared = desugar ast' env conMap
          liftIO $ putStrLn "=== Desugared ==="
          liftIO $ print $ ppr desugared
          kn <- runInfer env $ mapM (\(n, e) -> (n,) <$> KNormal.convert e) (_scDef desugared)
          liftIO $ putStrLn "=== K Normalized ==="
          liftIO $ print $ ppr kn

          -- cps <- mapM (\(n, e) -> (n,) <$> convAtomExp e) (_scDef desugared)
          -- liftIO $ putStrLn "=== CPS converted ==="
          -- liftIO $ print $ ppr cps
        Left _ -> pure ()
    Left err -> liftIO $ putStrLn $ errorBundlePretty err
