{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Language.Griff.Driver
import           Language.Griff.Prelude
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> compileFromPath filePath
    _          -> error "invalid arguments"
