{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Griff.Vm where

import           Control.Monad.IO.Class
import           Language.Griff.Instr

data Value = IntVal !Integer
           | ClosVal !Code !Env
           | BlockVal !Tag !Int ![Value]
           | EpsilonVal
  deriving (Show, Eq, Ord)

type Env = [Value]

type Stack = [Value]

next :: MonadIO m => Instr -> Code -> [Value] -> [Value] -> [Value] -> m (Code, [Value], [Value], [Value])
next (Ldi x) c e s r = return (c, e, IntVal x : s, r)
next (Access i) c e s r = return (c, e, e !! i : s, r)
next (Closure f) c e s r = return (c, e, ClosVal f e : s, r)
next Let c e (x:s) r = return (c, x:e, s, r)
next EndLet c (_:e) s r = return (c, e, s, r)
next (Test t _) _ e (IntVal 1 : s) r = return (t, e, s, r)
next (Test _ f) _ e (IntVal 0 : s) r = return (f, e, s, r)
next Add c e (IntVal x : IntVal y : s) r = return (c, e, IntVal (x + y) : s, r)
next Eq c e (IntVal x : IntVal y : s) r = return (c, e, IntVal (if x == y then 1 else 0) : s, r)
next (Block tag arity) c e s r = do
  let xs = take arity s
  return (c, e, BlockVal tag arity xs : drop arity s, r)
next (Field i) c e (BlockVal _ _ xs : s) r = return (c, e, xs !! i : s, r)
next (Invoke tag k) c e s@((BlockVal tag' _ _) : _) r
  | tag == tag' = return (k, e, s, r)
  | otherwise = return (c, e, s, r)
next Apply c e (ClosVal c' e' : v : s) r =
  return (c', v : ClosVal c' e' : e', s, ClosVal c e : r)
next TailApply _ _ (ClosVal c' e' : v : s) r =
  return (c', v : ClosVal c' e' : e', s, r)
next PushMark c e s r = return (c, e, EpsilonVal : s, r)
next Grab c e (EpsilonVal : s) (ClosVal c' e' : r) =
  return (c', e', ClosVal c e : s, r)
next Grab c e (v : s) r = return (c, v : ClosVal c e : e, s, r)
next Return _ _ (x : EpsilonVal : s) (ClosVal c' e' : r) =
  return (c', e', x : s, r)
next Return _ _ (ClosVal c' e' : v : s) r =
  return (c', v : ClosVal c' e' : e', s, r)
next c cs e s r = error $ "unreachable: " <> show (c, cs, e, s, r)

eval :: MonadIO m => Code -> [Value] -> [Value] -> [Value] -> m ([Value], [Value], [Value])
eval [] e s r = return (e, s, r)
eval (c:cs) e s r = do
  (cs', e', s', r') <- next c cs e s r
  eval cs' e' s' r'
