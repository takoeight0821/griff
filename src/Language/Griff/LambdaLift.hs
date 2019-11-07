{-# LANGUAGE NoImplicitPrelude #-}
module Language.Griff.LambdaLift (convert) where

import           Language.Griff.Core
import           Language.Griff.Id
import           Language.Griff.Prelude

{-
let rec f x = x + a
in f 10
=>
let rec f' a' x = x + a' (LLEnv: f -> f' a', a -> a')
in let f = f' a
in f 10                  (LLEnv: )

let rec f x = f (x + a)
in f 10
=>
let rec f' a' x = f' a' (x + a') (LLEnv: f -> f' a', a -> a')
in let f = f' a
in f 10                          (LLEnv: )

let rec f x = g (x + a)
and     g y = f y
in f 10
=>
let rec f' a' x = g' a' (x + a') (LLEnv: f -> f' a', a -> a', g -> g' a')
and     g' a' y = f' a' y        (LLEnv: f -> f' a', a -> a', g -> g' a')
in let f = f' a
in let g = g' a
in f 10                          (LLEnv: )
-}

data LLEnv = LLEnv { transedVars :: Map Id (Id, [Id]) }

convert = undefined
