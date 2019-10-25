{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Language.Griff.Driver
import           Language.Griff.Prelude
import           System.Environment
import           System.Exit

usage :: IO ()
usage = do
  putStr "usage: "
  exe <- getProgName
  putStr exe
  putStrLn " <file name>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> compileFromPath filePath
    _          -> usage >> exitFailure
