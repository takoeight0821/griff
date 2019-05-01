{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Language.Griff.L2I where

import           Data.List               (elemIndex)
import           Data.Maybe              (fromMaybe)
import           Language.Griff.Constant
import           Language.Griff.Id
import           Language.Griff.Instr
import qualified Language.Griff.Lambda   as L

dummyId :: Id
dummyId = Id "Dummy" (-1)

position :: Id -> [Id] -> Int
position x env =
  fromMaybe (error $ show x <> " is not defined")
  $ elemIndex x env

compile :: [Id] -> L.Exp -> [Instr]
compile env (L.Var x) = [Access (position x env)]
compile _   (L.Const (Int i)) = [Ldi i]
compile _   (L.Const (Bool b)) = [Ldi (if b then 1 else 0)]
compile _   e@L.Const{} = error $ "unimplemented: " <> show e
compile env (L.Op L.Add x y) =
  compile env y <> compile env x <> [Add]
compile env (L.Op L.Eq x y) =
  compile env y <> compile env x <> [Eq]
compile _ e@L.Op{} = error $ "unimplemented: " <> show e
compile env e@L.Apply{} =
  PushMark : concatMap (compile env) (applyToList e) <> [Apply]
compile env (L.Lambda x e) =
  Grab : compileTail (x:dummyId:env) e
compile env (L.Let x e1 e2) =
  compile env e1 <> [Let] <> compile (x:env) e2 <> [EndLet]
compile env (L.LetRec f (L.Lambda x e1) e2) =
  Closure (compileTail (x:f:env) e1) : Let : compile (f:env) e2 <> [EndLet]
compile _ e@L.LetRec{} = error $ "unimplemented: " <> show e
compile env (L.Select i e) =
  compile env e <> [Field i]
compile env (L.Pack tag es) =
  concatMap (compile env) (reverse es) <> [Block tag (length es)]
compile env (L.If c t f) =
  compile env c <> [Test (compile env t) (compile env f)]
compile env (L.Switch x es) =
  Access (position x env) : compileSwitch es
  where
    compileSwitch [] = []
    compileSwitch ((t, e) : xs) =
      Invoke t (compile env e) : compileSwitch xs

compileTail :: [Id] -> L.Exp -> [Instr]
compileTail env e@L.Apply{} =
  concatMap (compile env) (applyToList e) <> [TailApply]
compileTail env e@L.Lambda{} =
  compile env e
compileTail env (L.Let x e1 e2) =
  compile env e1 <> [Let] <> compileTail (x:env) e2
compileTail env (L.LetRec f (L.Lambda x e1) e2) =
  Closure (compileTail (x:f:env) e1) : [Let] <> compileTail (f:env) e2
compileTail env (L.If c t f) =
  compile env c <> [Test (compileTail env t) (compileTail env f)]
compileTail env (L.Switch x es) =
  Access (position x env) : compileSwitch es
  where
    compileSwitch [] = [Return]
    compileSwitch ((t, e) : xs) =
      Invoke t (compileTail env e) : compileSwitch xs
compileTail env e = compile env e <> [Return]

applyToList :: L.Exp -> [L.Exp]
applyToList (L.Apply f x) = x : applyToList f
applyToList f             = [f]
