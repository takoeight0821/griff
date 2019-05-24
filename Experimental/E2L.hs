{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Language.Griff.E2L where

import           Control.Monad
import           Language.Griff.ConsTable
import qualified Language.Griff.ELambda   as E
import           Language.Griff.Id
import           Language.Griff.Lambda
import           Language.Griff.Uniq

compile :: (HasConsTable f, HasUniq f) => E.Exp -> f Exp
compile (E.Const c) = return $ Const c
compile (E.Var x)   = return $ Var x
compile e@E.Apply{} = compileApply e []
compile (E.Constructor c) = do
  (tag, arity) <- lookupCons c
  xs <- replicateM arity (newId "v")
  return $ foldr Lambda (Pack tag (map Var xs)) xs
compile (E.Lambda x e) =
  Lambda x <$> compile e
compile (E.Let x e1 e2) =
  Let x <$> compile e1 <*> compile e2
compile (E.LetRec [(x, e1)] e2) =
  LetRec x <$> compile e1 <*> compile e2
compile (E.LetRec _ _) = error "Lambda.Exp must be closure converted"
compile (E.Case x cs@((E.ConstructorP{}, _) : _)) =
  Switch x <$> compileSwitch x cs
compile (E.Case x ((E.VarP x', e) : _)) =
  Let x' (Var x) <$> compile e
compile (E.Case _ ((E.ConstantP{}, _) : _)) =
  error "constant pattern is not supported"
compile (E.Case _ []) = error "no pattern"
compile (E.Prim p) = do
  x <- newId "x"
  y <- newId "y"
  return
    $ Lambda x
    $ Lambda y
    $ Op (compilePrim p) (Var x) (Var y)

compileSwitch :: (HasConsTable m, HasUniq m) => Id -> [(E.Pat, E.Exp)] -> m [(Tag, Exp)]
compileSwitch _ [] = return []
compileSwitch x ((E.ConstructorP c vs, e) : xs) = do
  (tag, _) <- lookupCons c
  e' <- buildLet 0 vs
  ((tag, e') :) <$> compileSwitch x xs
  where
    buildLet _ []     = compile e
    buildLet i (y:ys) = Let y (Select i (Var x)) <$> buildLet (i + 1) ys
compileSwitch x ((E.VarP x', e) : _) = do
  e' <- compile e
  return [(defaultTag, Let x' (Var x) e')]
compileSwitch _ _ = error "illegal pattern(case switch)"

compileApply :: (HasConsTable m, HasUniq m) => E.Exp -> [Exp] -> m Exp
compileApply (E.Prim p) [y, x] =
  return $ Op (compilePrim p) x y
compileApply E.Prim{} _ =
  error "illegal arguments(prim)"
compileApply (E.Constructor c) args = do
  (tag, _) <- lookupCons c
  return $ Pack tag $ reverse args
compileApply (E.Apply e1 e2) args = do
  e2' <- compile e2
  compileApply e1 (e2':args)
compileApply e args = do
  f <- compile e
  return $ foldl Apply f args

compilePrim :: E.Primitive -> Op
compilePrim E.Add  = Add
compilePrim E.Sub  = Sub
compilePrim E.Mul  = Mul
compilePrim E.Div  = Div
compilePrim E.Mod  = Mod
compilePrim E.FAdd = FAdd
compilePrim E.FSub = FSub
compilePrim E.FMul = FMul
compilePrim E.FDiv = FDiv
compilePrim E.Eq   = Eq
compilePrim E.Neq  = Neq
compilePrim E.Lt   = Lt
compilePrim E.Le   = Le
compilePrim E.Gt   = Gt
compilePrim E.Ge   = Add
compilePrim E.And  = And
compilePrim E.Or   = Or

wrapCase :: Id -> E.Pat -> E.Exp -> E.Exp
wrapCase x pat (E.Lambda p e) = E.Lambda p $ wrapCase x pat e
wrapCase x pat e              = E.Case x [(pat, e)]
