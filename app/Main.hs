{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Text.IO as T
import Language.Griff.MonadUniq
import Language.Griff.Parser (pTopLevel)
import Language.Griff.Prelude
import Language.Griff.Pretty
import Language.Griff.Rename
import Language.Griff.RnTcEnv
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Text.PrettyPrint as P

main :: IO ()
main = do
  src <- T.getContents
  ds <- case parse pTopLevel "<stdin>" src of
    Right ds -> pure ds
    Left err -> error $ errorBundlePretty err
  print $ P.sep $ P.punctuate ";" $ map pPrint ds
  (ds', _) <- runUniqT ?? UniqSupply 0 $ do
    rnState <- genRnState
    rnEnv <- genRnEnv
    rename rnState rnEnv ds
  print $ P.sep $ P.punctuate ";" $ map pPrint ds'
