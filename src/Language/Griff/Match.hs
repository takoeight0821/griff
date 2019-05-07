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

-- match
--   :: [Id] -- ^ scrutinees
--   -> [([Pat Id], Exp Id)]
--   -> Exp Id -- ^ default clause (normaly `Error`)
--   -> m (Exp Id)
match :: (HasConsTable m, HasUniq m)
  => [Id] -- ^ scrutinees
  -> [([Pat Id], Exp Id)] -- ^ pattern clauses
  -> Exp Id -- ^ expr when failed
  -> m (Exp Id)
match (u:us) [((VarP _ x):ps, e)] cont = do
  match us [( ps
            , rewrite (\case Var ss y | x == y -> Just (Var ss u)
                             _ -> Nothing) e
            )] cont
match (u:us) [((ConstructorP ss conName subPats):ps, e)] cont = do
  (_, arity) <- lookupCons conName

  -- compile sub patterns
  subUs <- replicateM arity (newId "u")
  e' <- match subUs [(subPats, e)] cont

  match us [(ps, Case ss (Var ss u) [(ConstructorP ss conName (map (VarP ss) subUs), e')] cont)] cont
match _us [([], e)] _ =
  -- empty rule
  -- TODO: compile error when us is not null
  return e -- empty rule
match [] [(_ps, e)] _ =
  -- empty rule
  -- TODO: compile error when ps is not null
  return e
match us ((ps, e):qs@(_:_)) cont = do
  e' <- match us qs cont
  match us [(ps, e)] e'
match _ [] cont           = return cont
