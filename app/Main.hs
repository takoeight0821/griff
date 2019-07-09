{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Griff.IR.LIR
import Language.Griff.Reducer.CodeGen
import qualified Data.ByteString.Char8 as BS
import LLVM.Module
import LLVM.Context

main :: IO ()
main = do
  let mod =  codeGen "hello" (Int 42)
  withContext $ \ctx -> do
    llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
    BS.putStrLn llvm
