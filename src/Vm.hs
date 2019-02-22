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
module Vm where

import           Capability.Reader
import           Capability.State
import           Control.Monad.Extra
import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , runReaderT
                                                )
import           Control.Monad.Trans            ( MonadTrans )
import           Data.Coerce
import           Data.IORef
import           Data.Map                       ( Map )
import           GHC.Generics                   ( Generic )
import qualified Data.Map                      as Map

data Ctx = Ctx {
    _code :: IORef Code,
    _env :: IORef Env,
    _stack :: IORef Stack
} deriving (Eq, Generic)

newtype MachineT (m :: * -> *) a = MachineT (Ctx -> m a)
    deriving (Functor, Applicative, Monad, MonadFail) via ReaderT Ctx m
    deriving MonadTrans via ReaderT Ctx
    deriving (HasState "code" Code) via
        ReaderIORef (Rename "_code" (Field "_code" ()
        (MonadReader (ReaderT Ctx m))))
    deriving (HasState "env" Env) via
        ReaderIORef (Rename "_env" (Field "_env" ()
        (MonadReader (ReaderT Ctx m))))
    deriving (HasState "stack" Stack) via
        ReaderIORef (Rename "_stack" (Field "_stack" ()
        (MonadReader (ReaderT Ctx m))))

runMachineT :: MonadIO m => MachineT m a -> m a
runMachineT (MachineT m) = do
    codeRef  <- liftIO $ newIORef $ Code []
    envRef   <- liftIO $ newIORef $ Env []
    stackRef <- liftIO $ newIORef $ Stack []
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
           | Add -- ^ 整数の和
           | Eq -- ^ 整数の等価判定
    deriving (Show, Eq, Ord, Generic)

newtype Code = Code [Instr]
    deriving (Show, Eq, Ord, Generic, Semigroup)

newtype Env = Env [Value]
    deriving (Show, Eq, Ord, Generic, Semigroup)

newtype Stack = Stack [Value]
    deriving (Show, Eq, Ord, Generic)

data Value = IntV Integer
           | BoolV Bool
           | ClosureV Code Env
    deriving (Show, Eq, Ord, Generic)

push :: HasState "stack" Stack m => Value -> m ()
push x = modify @"stack" $ \(Stack xs) -> Stack $ x : xs

pop :: (HasState "stack" Stack m, MonadFail m) => m Value
pop = do
    Stack (x : xs) <- get @"stack"
    put @"stack" $ Stack xs
    return x

lookupEnv :: HasState "env" Env f => Int -> f Value
lookupEnv i = (\(Env xs) -> xs !! i) <$> get @"env"

transition
    :: ( MonadFail m
       , HasState "code" Code m
       , HasState "env" Env m
       , HasState "stack" Stack m
       )
    => Instr
    -> m ()
transition (Int     n) = push $ IntV n
transition (Bool    b) = push $ BoolV b
transition (Access  i) = push =<< lookupEnv i
transition (Closure c) = push =<< ClosureV c <$> get @"env"
transition Apply       = do
    ClosureV c' env' <- pop -- 呼び出す関数
    v                <- pop -- 引数

    push =<< ClosureV <$> get @"code" <*> get @"env" -- 戻り先をプッシュ

    put @"env" $ Env [v, ClosureV c' env'] <> env'
    put @"code" c'
transition Return = do
    v              <- pop
    ClosureV c env <- pop
    put @"code" c
    put @"env" env
    push v
transition Let = do
    v <- pop
    modify @"env" (\(Env xs) -> Env $ v : xs)
transition EndLet       = modify @"env" (\(Env (_ : xs)) -> Env xs)
transition (Test c1 c2) = do
    BoolV v <- pop
    modify @"code" ((if v then c1 else c2) <>)
transition Add = do
    IntV x <- pop
    IntV y <- pop
    push $ IntV $ x + y
transition Eq = do
    IntV x <- pop
    IntV y <- pop
    push $ BoolV $ x == y

eval :: (MonadIO m, MonadFail m) => MachineT m ()
eval = whileM $ do
    Code cs <- get @"code"
    case cs of
        [] -> return False
        (c:cs) -> do
            put @"code" $ Code cs
            transition c
            return True