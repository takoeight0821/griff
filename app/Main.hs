module Main where

import Language.Griff.Instr
import Language.Griff.Vm

consTest :: [Instr]
consTest =
  [ Block 0 0
  , Ldi 42
  , Block 1 2
  , Invoke 0 [ Ldi 0 ]
  , Invoke 1 [ Ldi 1 ]
  , Ldi 2
  ]

main :: IO ()
main = print =<< eval consTest [] [] []
