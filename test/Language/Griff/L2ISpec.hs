{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Griff.L2ISpec where

import           Language.Griff.Constant
import           Language.Griff.Id
import           Language.Griff.Instr    as I
import           Language.Griff.L2I
import           Language.Griff.Lambda   as L
import           Test.Hspec

sumLam :: Exp
sumLam =
  LetRec sum (Lambda x
              (Lambda acc
               (If (Op L.Eq (Var x) (Const $ Int 0))
                (Var acc)
                (L.Apply
                 (L.Apply (Var sum) (Op L.Add (Var x) (Const $ Int (-1))))
                 (Op L.Add (Var acc) (Var x))))))
  (L.Apply (L.Apply (Var sum) (Const $ Int 10)) (Const $ Int 0))
  where
    sum = Id "sum" 0
    x = Id "x" 1
    acc = Id "acc" 2

sumInstr :: [Instr]
sumInstr =
  [ Closure
    [ Grab
    , Ldi 0
    , Access 2
    , I.Eq
    , Test
      [Access 0, Return]
      [Access 2, Access 0, I.Add, Ldi (-1), Access 2, I.Add, Access 3, TailApply]
    ]
  , I.Let
  , PushMark
  , Ldi 0
  , Ldi 10
  , Access 0
  , I.Apply
  , EndLet
  ]

spec :: SpecWith ()
spec = do
  describe "lambda to instr" $ do
    it "sum" $ do
      compile [] sumLam `shouldBe` sumInstr
