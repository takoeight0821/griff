{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Language.Griff.E2L.PatCase where

import           Language.Griff.ELambda
import           Language.Griff.Id
import           Language.Griff.Uniq

patCase :: HasUniq m => Exp -> m Exp
patCase e@Const{} = return e
patCase e@Var{} = return e
patCase e@Constructor{} = return e
patCase (Apply e1 e2) = Apply <$> patCase e1 <*> patCase e2
patCase (Lambda p e) = do
  x <- newId "_lambda_pat"
  e' <- patCase e
  return $ Lambda (VarP x) $ wrapCase x p e'
patCase (Let p e1 e2) = do
  x <- newId "_let_pat"
  e1' <- patCase e1
  e2' <- patCase e2
  return $ Let (VarP x) e1' $ wrapCase x p e2'
patCase (LetRec p e1 e2) = do
  x <- newId "_letrec_pat"
  e1' <- patCase e1
  e2' <- patCase e2
  return $ LetRec (VarP x) (wrapCase x p e1') (wrapCase x p e2')
patCase (Case x clauses) =
  Case x <$> mapM (\(p, e) -> (p,) <$> patCase e) clauses
patCase e@Prim{} = return e

wrapCase :: Id -> Pat -> Exp -> Exp
wrapCase x pat (Lambda p e) = Lambda p $ wrapCase x pat e
wrapCase x pat e            = Case x [(pat, e)]
