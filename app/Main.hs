module Main where

import           Language.Griff.Instr
import           Language.Griff.Vm

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

{-
  C(let cons = fun x -> fun xs -> x : xs in _, [])
= C(fun x -> fun xs -> x : xs, []); Let; C(_, [cons])
= Closure T(fun xs -> x : xs, [x, _]); Let; C(_, [cons])
= Closure [Grab; T(x : xs, [xs, _, x, _])]; Let; C(_, [cons])
= Closure [Grab; C(xs, [xs, _, x, _]); C(x, [xs, _, x, _]); Block 1 2; TailApply]; Let; C(_, [cons])
= Closure [Grab; Access 0; Access 2; Block 1 2; TailApply]; Let; C(_, [cons])
= Block 1 2

  C( let rec f n =
       if n == 0
       then []
       else cons n (f (n - 1))
     in f 10
   , [cons])
= C( Closure T( if n == 0
                then []
                else cons n (f (n - 1))
              , [n, f, cons]);
     Let;
     T(f 10, [f, cons])
   , [cons])
= C( Closure [ C(n == 0, [n, f, cons]);
               Test
                 (T([], [n, f, cons]))
                 (T(cons n (f (n - 1)), [n, f, cons]))];
     Let;
     C(10, [f, cons]);
     C(f, [f, cons]);
     TailApply
   , [cons])
= C( Closure [ C(0, [n, f, cons]);
               C(n, [n, f, cons]);
               Eq;
               Test
                 [Block 0 0; Return]
                 [C(f (n - 1), [n, f, cons]);
                  C(n, [n, f, cons]);
                  Block 1 2;
                  Return]
     Let;
     Ldi 10;
     Access 0;
     TailApply
   , [cons])
= C( Closure [ Ldi 0;
               Access 0;
               Eq;
               Test
                 [Block 0 0; Return]
                 [PushMark;
                  C(n - 1, [n, f, cons]);
                  C(f, [n, f, cons]);
                  Apply;
                  Access 0;
                  Block 1 2;
                  Return]
     Let;
     Ldi 10;
     Access 0;
     TailApply
   , [cons])
= C( Closure [ Ldi 0;
               Access 0;
               Eq;
               Test
                 [Block 0 0; Return]
                 [PushMark;
                  Ldi (-1);
                  Access 0;
                  Add;
                  Access 1;
                  Apply;
                  Access 0;
                  Block 1 2;
                  Return]
     Let;
     Ldi 10;
     Access 0;
     TailApply
   , [cons])
-}
consTest2 :: [Instr]
consTest2 =
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
   , Ldi 100
   , Access 0
   , Apply
   , EndLet
  ]

main :: IO ()
main = do
  print =<< eval consTest [] [] []
  print =<< eval sumTest [] [] []
  print =<< eval consTest2 [] [] []
