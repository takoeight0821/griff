{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Language.Griff.KNormal (convert) where

import           Control.Effect
import           Control.Monad.Fail
import           Data.Bifunctor
import           Language.Griff.Core
import           Language.Griff.Id
import           Language.Griff.Typing.Monad

convert :: (Carrier sig m, InferEff sig, MonadFail m) => Exp -> m Exp
convert e = flatten <$> conv e

insertLet :: (Carrier sig m, InferEff sig, MonadFail m) => Exp -> (Exp -> m Exp) -> m Exp
insertLet (Var x) k = k (Var x)
insertLet (Int x) k = k (Int x)
insertLet (Char x) k = k (Char x)
insertLet (String x) k = k (String x)
insertLet (Bool x) k = k (Bool x)
insertLet x k = do
  x' <- newId "k"
  addScheme =<< (x',) <$> schemeOf x
  Let x' <$> conv x <*> k (Var x')

conv :: (Carrier sig m, InferEff sig, MonadFail m) => Exp -> m Exp
conv (Record r) = go r []
  where go [] ys          = pure $ Record ys
        go ((l, x):xs) ys = insertLet x $ \x' -> go xs ((l, x'):ys)
conv (Proj l e t) = insertLet e $ \e' -> pure $ Proj l e' t
conv (Apply e1 e2) = insertLet e1 $ \e1' -> insertLet e2 $ \e2' -> pure $ Apply e1' e2'
conv (Lambda x e) = Lambda x <$> conv e
conv (Let x e1 e2) = Let x <$> conv e1 <*> conv e2
conv (LetRec ds e) = LetRec <$> mapM (\(n, v) -> (n,) <$> conv v) ds <*> conv e
conv (Prim op es) = go es []
  where go [] ys     = pure $ Prim op ys
        go (x:xs) ys = insertLet x $ \x' -> go xs (x':ys)
conv (Case e cs) = Case <$> conv e <*> mapM (\(p, v) -> (p,) <$> conv v) cs
conv x = pure x

flatten :: Exp -> Exp
flatten (Lambda x e) = Lambda x (flatten e)
flatten (Let x e1 e2) = insert (flatten e1)
  where
    insert (Let y e3 e4) = Let y e3 (insert e4)
    insert (LetRec ds e) = LetRec ds (insert e)
    insert e             = Let x e (flatten e2)
flatten (LetRec ds e) = LetRec (map (second flatten) ds) e
flatten (Case e cs) = Case (flatten e) (map (second flatten) cs)
flatten e = e