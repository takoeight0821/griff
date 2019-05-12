{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Griff.Match where

import           Control.Lens.Plated
import           Control.Monad
import           Language.Griff.ConsTable
import           Language.Griff.Id
import           Language.Griff.Syntax
import           Language.Griff.Uniq

-- 入れ子になったパターンマッチの展開

match :: (HasConsTable m, HasUniq m)
  => [Id] -- ^ scrutinees
  -> [([Pat Id], Exp Id)] -- ^ pattern clauses
  -> Exp Id -- ^ expr when failed
  -> m (Exp Id)
match (u:us) [((VarP _ x):ps, e)] cont = do
  match us [( ps
            , rewrite (\case { Var ss y | x == y -> Just (Var ss u)
                             ; _ -> Nothing}) e
            )] cont
match (u:us) [((ConstructorP ss conName subPats):ps, e)] cont = do
  (_, arity) <- lookupCons conName
  subUs <- replicateM arity (newId "u")

  when (length subUs /= length subPats) $
    error "the length of subUs and subPats must be equal"

  match (subUs <> us) [(subPats <> ps,
                        Case ss (Var ss u)
                         [(ConstructorP ss conName (map (VarP ss) subUs), e)]
                         cont)] cont
match (u:us) [((IntP ss i):ps, e)] cont =
  match us [(ps, If ss (BinOp ss Eq (Var ss u) (Int ss i)) e cont)] cont
match (u:us) [((BoolP ss b):ps, e)] cont =
  match us [(ps, If ss (BinOp ss Eq (Var ss u) (Bool ss b)) e cont)] cont
match (u:us) [((CharP ss c):ps, e)] cont =
  match us [(ps, If ss (BinOp ss Eq (Var ss u) (Char ss c)) e cont)] cont
match (u:us) [((StringP ss s):ps, e)] cont =
  match us [(ps, If ss (BinOp ss Eq (Var ss u) (String ss s)) e cont)] cont
match [] [([], e)] _ = return e -- empty rule
match [] _ _ = error "the length of ps must be 0"
match _ [([], _)] _ = error "the length of us must be 0"
match us ((ps, e):qs@(_:_)) cont = do
  e' <- match us qs cont
  when (length us /= length ps) $
    error "the length of us and ps must be equal"
  match us [(ps, e)] e'
match _ [] cont = return cont
