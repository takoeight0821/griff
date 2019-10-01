{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Effect
import           Control.Monad.IO.Class
import           Data.Outputable
import           Data.String
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
      liftIO $ print $ ppr ast
      ast' <- rename ast
      typeEnv <- runInfer mempty (infer ast')
      liftIO $ print $ ppr ast'
      liftIO $ print $ ppr typeEnv
    Left err -> liftIO $ putStrLn $ errorBundlePretty err
