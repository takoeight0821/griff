{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
module Language.Griff.Monad where

import           Capability.Reader
import           Capability.State
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Data.IORef
import           GHC.Generics
import           Language.Griff.Id

data Ctx = Ctx { _uniq :: IORef Int
               , _consTable :: IORef [(Id, (Int, Int))]
               } deriving Generic

newtype GriffT m a = GriffT (ReaderT Ctx m a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "uniq" Int) via ReaderIORef (Rename "_uniq" (Field "_uniq" () (MonadReader (ReaderT Ctx m))))
  deriving (HasState "consTable" [(Id, (Int, Int))]) via ReaderIORef (Rename "_consTable" (Field "_consTable" () (MonadReader (ReaderT Ctx m))))

runGriffT :: MonadIO m => GriffT m a -> m a
runGriffT (GriffT m) = do
  i <- liftIO $ newIORef 0
  c <- liftIO $ newIORef []
  runReaderT m $ Ctx { _uniq = i, _consTable = c }