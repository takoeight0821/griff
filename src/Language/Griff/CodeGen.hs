{-# LANGUAGE FlexibleContexts #-}
module Language.Griff.CodeGen where

import Control.Monad.State
import Language.Griff.Syntax
import Language.Griff.Id

codegen :: [(Id, [Id], Exp Id)] -> IO ()
codegen [(_, _, e)] = do
  putStrLn ".intel_syntax noprefix"
  putStrLn ".text"

  putStrLn ".global main"
  putStrLn "main:\n"
  emit "push rbp"
  emit "mov rbp, rsp"
  emit "sub rsp, 0"

  evalStateT (cgExp e >> pop "rax") 0

  emit "leave"
  emit "ret"


emit :: MonadIO m => String -> m ()
emit x = liftIO $ putStrLn $ "\t" <> x

push :: (MonadIO m, MonadState Int m) => String -> m ()
push x = do
  emit ("push " <> x)
  modify (+8)

pop :: (MonadIO m, MonadState Int m) => String -> m ()
pop x = do
  emit ("pop " <> x)
  modify (\n -> n - 8)

cgExp :: (MonadIO m, MonadState Int m) => Exp Id -> m ()
cgExp (Int _ i) = push (show i)
