{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Strict, StrictData #-}
module Vm
    ( Ctx(..)
    , MachineT(..)
    , Code
    , Env
    , Stack
    , Instr(..)
    , Value(..)
    , runMachineT
    , load
    , eval
    , dumpCtx
    )
where

import           Capability.Reader
import           Capability.State
import           Control.Monad.Extra
import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Control.Monad.Trans            ( MonadTrans )
import           Data.IORef
import qualified Data.Vector.Fixed             as V
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           GHC.Generics                   ( Generic )

data Ctx = Ctx {
    _code :: IORef Code,
    _env :: IORef Env,
    _stack :: IORef Stack
} deriving (Eq, Generic)

type CtxField field m
    = ReaderIORef (Rename field (Field field () (MonadReader (ReaderT Ctx m))))

newtype MachineT (m :: * -> *) a = MachineT (Ctx -> m a)
    deriving (Functor, Applicative, Monad, MonadFail) via ReaderT Ctx m
    deriving MonadTrans via ReaderT Ctx
    deriving (HasState "code" Code) via CtxField "_code" m
    deriving (HasState "env" Env) via CtxField "_env" m
    deriving (HasState "stack" Stack) via CtxField "_stack" m

runMachineT :: MonadIO m => MachineT m a -> m a
runMachineT (MachineT m) = do
    codeRef  <- liftIO $ newIORef []
    envRef   <- liftIO $ newIORef []
    stackRef <- liftIO $ newIORef []
    m $ Ctx { _code = codeRef, _env = envRef, _stack = stackRef }

dumpCtx
    :: (HasState "code" Code m, HasState "env" Env m, HasState "stack" Stack m)
    => m (Map String String)
dumpCtx = do
    code  <- get @"code"
    env   <- get @"env"
    stack <- get @"stack"
    return $ Map.fromList
        [("code", show code), ("env", show env), ("stack", show stack)]

data Instr = Int Integer -- ^ 整数をスタックにプッシュ
           | Bool Bool -- ^ 真偽値をスタックにプッシュ
           | Access Int -- ^ 環境のi番目の値をスタックにプッシュ
           | Closure Code -- ^ クロージャを作成してスタックにプッシュ
           | Apply -- ^ スタックトップの値がクロージャなら、2番目の値に適用する
           | Return -- ^ 関数の呼び出し元に戻る
           | Let -- ^ スタックトップの値を環境の先頭にプッシュ
           | EndLet -- ^ 環境の先頭の値を取り除く
           | Test Code Code -- ^ スタックトップの値がtrueなら1つ目、falseなら2つ目の引数を実行する
           | Op Op
    deriving (Show, Eq, Ord, Generic)

data Op = IAdd | ISub | IMul | IDiv | IRem
        -- | FAdd | FSub | FMul | FDiv
        | IEq | INeq | ILt | ILe | IGt | IGe
        | Not
    deriving (Show, Eq, Ord, Generic)

type Code = [Instr]

type Env = [Value]

type Stack = [Value]

data Value = IntV Integer
           | BoolV Bool
           | ClosureV Code Env
    deriving (Show, Eq, Ord, Generic)

push :: HasState "stack" Stack m => Value -> m ()
push x = modify @"stack" (x :)

pop :: (HasState "stack" Stack m, MonadFail m) => m Value
pop = do
    (x : xs) <- get @"stack"
    put @"stack" xs
    return x

viewEnv :: HasState "env" Env f => Int -> f Value
viewEnv i = (!! i) <$> get @"env"

-- | 状態遷移関数
transition
    :: ( HasState "code" Code m
       , HasState "env" Env m
       , HasState "stack" Stack m
       , MonadFail m
       )
    => Instr
    -> m ()
transition (Int     n) = push $ IntV n
transition (Bool    b) = push $ BoolV b
transition (Access  i) = push =<< viewEnv i
transition (Closure c) = push =<< ClosureV c <$> get @"env"
transition Apply       = modifyCtx $ \code env (ClosureV code' env', v) ->
    (const code', const ([v, ClosureV code' env'] <> env'), [ClosureV code env])
transition Return =
    modifyCtx $ \_ _ (v, ClosureV code env) -> (const code, const env, [v])
transition Let    = modifyCtx $ \_ _ (V.Only v) -> (id, (v :), [])
transition EndLet = modify @"env" tail
transition (Test c1 c2) =
    modifyCtx $ \_ _ (V.Only (BoolV v)) -> ((ifThenElse v c1 c2 <>), id, [])
transition (Op op) = case op of
    IAdd -> calculate $ \(IntV x, IntV y) -> [IntV $ x + y]
    ISub -> calculate $ \(IntV x, IntV y) -> [IntV $ x - y]
    IMul -> calculate $ \(IntV x, IntV y) -> [IntV $ x * y]
    IDiv -> calculate $ \(IntV x, IntV y) -> [IntV $ x `quot` y] -- truncated toward zero
    IRem -> calculate $ \(IntV x, IntV y) -> [IntV $ x `rem` y]
    IEq  -> calculate $ \(IntV x, IntV y) -> [BoolV $ x == y]
    INeq -> calculate $ \(IntV x, IntV y) -> [BoolV $ x /= y]
    ILt  -> calculate $ \(IntV x, IntV y) -> [BoolV $ x < y]
    ILe  -> calculate $ \(IntV x, IntV y) -> [BoolV $ x <= y]
    IGt  -> calculate $ \(IntV x, IntV y) -> [BoolV $ x > y]
    IGe  -> calculate $ \(IntV x, IntV y) -> [BoolV $ x >= y]
    Not  -> calculate $ \(V.Only (BoolV x)) -> [BoolV $ not x]

calculate
    :: ( HasState "code" Code m
       , HasState "env" Env m
       , HasState "stack" Stack m
       , MonadFail m
       , V.Vector v Value
       )
    => (v Value -> Stack)
    -> m ()
calculate f = modifyCtx (\_ _ vs -> (id, id, f vs))

modifyCtx
    :: ( HasState "code" Code m
       , HasState "env" Env m
       , HasState "stack" Stack m
       , MonadFail m
       , V.Vector v Value
       )
    => (Code -> Env -> v Value -> (Code -> Code, Env -> Env, Stack))
    -> m ()
modifyCtx update = do
    (updateCode, updateEnv, pushes) <-
        update <$> get @"code" <*> get @"env" <*> V.replicateM pop
    modify @"code" updateCode
    modify @"env" updateEnv
    modify @"stack" (pushes <>)

eval
    :: ( HasState "code" Code m
       , HasState "env" Env m
       , HasState "stack" Stack m
       , MonadFail m
       )
    => m ()
eval = whileM $ do
    cs <- get @"code"
    case cs of
        []             -> return False
        (instr : rest) -> do
            put @"code" rest
            transition instr
            return True

load :: HasState "code" Code m => Code -> m ()
load = put @"code"

ifThenElse :: Bool -> p -> p -> p
ifThenElse c t f = if c then t else f
