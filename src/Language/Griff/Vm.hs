{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Griff.Vm where

import           Control.Lens
import           Control.Monad.Extra
import           Control.Monad.Fail
import           Control.Monad.State
import qualified Data.Vector.Fixed   as V

data Value = IntVal Integer
           | BoolVal Bool
           | ClosVal Code Env
  deriving (Show, Eq, Ord)

type Env = [Value]

type Stack = [Value]

type Code = [Instr]

data Instr = Ldi Integer
           | Ldb Bool
           | Access Int
           | Closure Code
           | Apply
           | Return
           | Let
           | EndLet
           | Test Code Code
           | Add
           | Eq
  deriving (Show, Eq, Ord)

data Ctx = Ctx { _codeL  :: Code
               , _stackL :: Stack
               , _envL   :: Env
               }
  deriving (Show, Eq, Ord)

makeLenses ''Ctx

newtype MachineT m a = MachineT (StateT Ctx m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Ctx, MonadFail)

runMachineT :: MachineT m a -> Ctx -> m (a, Ctx)
runMachineT m =
  runStateT $ m ^. coerced

push :: MonadState Ctx m => Value -> m ()
push x = stackL %= (x:)

pop :: (MonadState Ctx m, MonadFail m) => m Value
pop = do
  (x : xs) <- use stackL
  stackL .= xs
  return x

next :: (MonadState Ctx m, MonadFail m) => Instr -> m ()
next (Ldi n) = push $ IntVal n
next (Ldb b) = push $ BoolVal b
next (Access i) = push . (!! i) =<< use envL
next (Closure c) = push =<< ClosVal c <$> use envL
next Apply = modifyCtx $ \code env (ClosVal code' env', v) -> (const code', const ([v, ClosVal code' env'] <> env'), [ClosVal code env])
next Return = modifyCtx $ \_ _ (v, ClosVal code env) -> (const code, const env, [v])
next Let = modifyCtx $ \_ _ (V.Only v) -> (id, (v:), [])
next EndLet = envL %= tail
next (Test c1 c2) =
  modifyCtx $ \_ _ (V.Only (BoolVal b)) -> (if b then (c1 <>) else (c2 <>), id, [])
next Add = calculate $ \(IntVal x, IntVal y) -> [IntVal $ x + y]
next Eq = calculate $ \(IntVal x, IntVal y) -> [BoolVal $ x == y]

calculate :: (MonadState Ctx m, V.Vector v Value, MonadFail m) => (v Value -> Stack) -> m ()
calculate f = modifyCtx (\_ _ vs -> (id, id, f vs))

modifyCtx :: (MonadState Ctx m, V.Vector v Value, MonadFail m) => (Code -> Env -> v Value -> (Code -> Code, Env -> Env, Stack)) -> m ()
modifyCtx update = do
  (updateCode, updateEnv, pushes) <-
    update <$> use codeL <*> use envL <*> V.replicateM pop
  codeL %= updateCode
  envL %= updateEnv
  stackL %= (pushes <>)

eval :: (MonadState Ctx m, MonadFail m) => m ()
eval = whileM $ do
  cs <- use codeL
  case cs of
    [] -> return False
    (instr : rest) -> do
      codeL .= rest
      next instr
      return True

test :: MonadFail m => m ((), Ctx)
test = runMachineT eval $ Ctx
  { _codeL =
      [Closure [Ldi 1, Access 0, Eq,
                Test
                 [Ldi 1]
                 [Ldi $ -1, Access 0, Add, Access 1, Apply, Access 0, Add],
                Return],
        Let, Ldi 100, Access 0, Apply, EndLet]
  , _envL = []
  , _stackL = []
  }
