{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as T
import Language.Griff.Parser (pTopLevel)
import Text.Megaparsec (parse)
import Language.Griff.Pretty
import qualified Text.PrettyPrint as P

main :: IO ()
main = do
  src <- T.getContents
  print $ P.sep . P.punctuate ";" . map pPrint <$> parse pTopLevel "<stdin>" src
