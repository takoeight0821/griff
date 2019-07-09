{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Griff.Reducer.SynToLIR
import Language.Griff.Reducer.CodeGen
import qualified Data.ByteString.Char8 as BS
import LLVM.Module
import LLVM.Context
import Text.Megaparsec
import Language.Griff.Parser
import Data.String

main :: IO ()
main = do
  src <- getContents
  let Right ast = parse pExp "hello" (fromString src)
  let mod =  codeGen "hello" $ synToLIR ast
  withContext $ \ctx -> do
    llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
    BS.putStrLn llvm
