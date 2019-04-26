module Main where

import           Language.Griff.Instr
import           Language.Griff.Vm
import           System.Environment

sumTest :: [Instr]
sumTest =
  [ Closure [ Grab, Ldi 0, Access 2, Eq
            , Test [ Access 0, Return ] [ Access 0, Access 2, Add, Ldi (-1), Access 2, Add, Access 3, TailApply ]
            ]
  , Let
  , PushMark
  , Ldi 0
  , Ldi 100000
  , Access 0
  , Apply
  , EndLet
  ]

consTest :: [Instr]
consTest =
  [ Block 0 0
  , Ldi 42
  , Block 1 2
  , Invoke 0 [ Ldi 0 ]
  , Invoke 1 [ Ldi 1 ]
  , Ldi 2
  ]

consTest2 :: Integer -> [Instr]
consTest2 n =
  [ Closure [ Ldi 0
            , Access 0
            , Eq
            , Test
              [ Block 0 0, Return ]
              [ PushMark, Ldi (-1), Access 0, Add
              , Access 1, Apply, Access 0, Block 1 2
              , Return
              ]
            ]
   , Let
   , PushMark
   , Ldi n
   , Access 0
   , Apply
   , EndLet
  ]

main :: IO ()
main = do
  (n:_) <- getArgs
  print =<< eval consTest [] [] []
  print =<< eval sumTest [] [] []
  print =<< eval (consTest2 $ read n) [] [] []
