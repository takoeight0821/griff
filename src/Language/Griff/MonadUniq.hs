{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Griff.MonadUniq where

import Control.Monad.Fix
import qualified Control.Monad.Trans.State.Lazy as Lazy
import LLVM.IRBuilder (IRBuilderT, ModuleBuilderT)
import Language.Griff.Prelude

newtype UniqSupply = UniqSupply {uniqSupply :: Int}

class Monad m => MonadUniq m where
  getUniqSupply :: m UniqSupply
  default getUniqSupply :: (MonadTrans t, MonadUniq m1, m ~ t m1) => m UniqSupply
  getUniqSupply = lift getUniqSupply
  getUniq :: m Int
  default getUniq :: (MonadTrans t, MonadUniq m1, m ~ t m1) => m Int
  getUniq = lift getUniq

newtype UniqT m a = UniqT {unUniqT :: StateT UniqSupply m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadFix, MonadFail)

runUniqT :: UniqT m a -> UniqSupply -> m (a, UniqSupply)
runUniqT (UniqT m) = runStateT m

instance Monad m => MonadUniq (UniqT m) where
  getUniqSupply = UniqT get
  getUniq = do
    i <- uniqSupply <$> getUniqSupply
    UniqT $ modify (\s -> s {uniqSupply = i + 1})
    pure i

instance MonadUniq m => MonadUniq (ReaderT r m)

instance MonadUniq m => MonadUniq (ExceptT e m)

instance MonadUniq m => MonadUniq (StateT s m)

instance MonadUniq m => MonadUniq (Lazy.StateT s m)

instance MonadUniq m => MonadUniq (WriterT w m)

instance MonadUniq m => MonadUniq (ModuleBuilderT m)

instance MonadUniq m => MonadUniq (IRBuilderT m)
