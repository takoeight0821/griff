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
    , Code(..)
    , Env(..)
    , Stack(..)
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
import           Data.Coerce                    ( coerce )
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
    deriving (HasState Code Code) via CtxField "_code" m
    deriving (HasState Env Env) via CtxField "_env" m
    deriving (HasState Stack Stack) via CtxField "_stack" m

runMachineT :: MonadIO m => MachineT m a -> m a
runMachineT (MachineT m) = do
    codeRef  <- liftIO $ newIORef $ Code []
    envRef   <- liftIO $ newIORef $ Env []
    stackRef <- liftIO $ newIORef $ Stack []
    m $ Ctx { _code = codeRef, _env = envRef, _stack = stackRef }

dumpCtx
    :: (HasState Code Code m, HasState Env Env m, HasState Stack Stack m)
    => m (Map String String)
dumpCtx = do
    code  <- get @Code
    env   <- get @Env
    stack <- get @Stack
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
           | Add -- ^ 整数の和
           | Eq -- ^ 整数の等価判定
    deriving (Show, Eq, Ord, Generic)

newtype Code = Code [Instr]
    deriving (Show, Eq, Ord, Generic, Semigroup)

newtype Env = Env [Value]
    deriving (Show, Eq, Ord, Generic, Semigroup)

newtype Stack = Stack [Value]
    deriving (Show, Eq, Ord, Generic, Semigroup)

data Value = IntV Integer
           | BoolV Bool
           | ClosureV Code Env
    deriving (Show, Eq, Ord, Generic)

push :: HasState Stack Stack m => Value -> m ()
push x = modify @Stack $ \(Stack xs) -> Stack $ x : xs

pop :: (HasState Stack Stack m, MonadFail m) => m Value
pop = do
    Stack (x : xs) <- get @Stack
    put @Stack $ Stack xs
    return x

viewEnv :: HasState Env Env f => Int -> f Value
viewEnv i = (\(Env xs) -> xs !! i) <$> get @Env

-- | 状態遷移関数
transition
    :: ( MonadFail m
       , HasState Code Code m
       , HasState Env Env m
       , HasState Stack Stack m
       )
    => Instr
    -> m ()
transition (Int     n) = push $ IntV n
transition (Bool    b) = push $ BoolV b
transition (Access  i) = push =<< viewEnv i
transition (Closure c) = push =<< ClosureV c <$> get @Env
transition Apply       = modifyCtx $ \code env (ClosureV code' env', v) ->
    ( const code'
    , const (Env [v, ClosureV code' env'] <> env')
    , [ClosureV code env]
    )
transition Return =
    modifyCtx $ \_ _ (v, ClosureV code env) -> (const code, const env, [v])
transition Let          = modifyCtx $ \_ _ (V.Only v) -> (id, (Env [v] <>), [])
transition EndLet       = modify @Env $ coerce $ tail @Value
transition (Test c1 c2) = modifyCtx
    $ \_ _ (V.Only (BoolV v)) -> ((ifThenElse v c1 c2 <>), id, [])
transition Add = calculate $ \(IntV x, IntV y) -> [IntV $ x + y]
transition Eq  = calculate $ \(IntV x, IntV y) -> [BoolV $ x == y]

calculate
    :: ( V.Vector v Value
       , HasState Code Code m
       , HasState Env Env m
       , HasState Stack Stack m
       , MonadFail m
       )
    => (v Value -> [Value])
    -> m ()
calculate f = modifyCtx (\_ _ vs -> (id, id, f vs))

modifyCtx
    :: ( V.Vector v Value
       , HasState Code Code m
       , HasState Env Env m
       , HasState Stack Stack m
       , MonadFail m
       )
    => (Code -> Env -> v Value -> (Code -> Code, Env -> Env, [Value]))
    -> m ()
modifyCtx update = do
    code <- get @Code
    env  <- get @Env
    vs   <- V.replicateM pop
    let (updateCode, updateEnv, pushes) = update code env vs
    modify @Code updateCode
    modify @Env updateEnv
    modify @Stack (Stack pushes <>)

eval
    :: ( MonadFail m
       , HasState Code Code m
       , HasState Env Env m
       , HasState Stack Stack m
       )
    => m ()
eval = whileM $ do
    Code cs <- get @Code
    case cs of
        []             -> return False
        (instr : rest) -> do
            put @Code $ Code rest
            transition instr
            return True

load :: HasState Code Code m => [Instr] -> m ()
load = put @Code . Code

ifThenElse :: Bool -> p -> p -> p
ifThenElse c t f = if c then t else f