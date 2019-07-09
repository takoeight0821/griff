{-# LANGUAGE OverloadedStrings #-}
module Language.Griff.Reducer.CodeGen where

import           Data.ByteString.Short
import           Language.Griff.IR.LIR
import           LLVM.AST              (Module)
import           LLVM.AST.Operand      (Operand)
import           LLVM.AST.Type         as AST
import           LLVM.IRBuilder        as IRBuilder

codeGen :: ShortByteString -> Exp -> LLVM.AST.Module
codeGen file lir = buildModule file $
  function "main" [] AST.i32 $ \_ ->
    ret =<< cgExp lir

cgExp :: Applicative f => Exp -> f Operand
cgExp (Int x) = int32 x
