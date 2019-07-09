{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
module Language.Griff.Monad where

import           Capability.Reader
import           Capability.State
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader      (ReaderT, runReaderT)
import qualified Control.Monad.State.Class as MTL
import           Data.IORef
import           Data.Text
import           GHC.Generics

data Ctx = Ctx { _uniq     :: IORef Int
               , _fileName :: Text
               } deriving Generic

newtype GriffT m a = GriffT (ReaderT Ctx m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFail)
  deriving (HasState "uniq" Int) via ReaderIORef (Rename "_uniq" (Field "_uniq" () (MonadReader (ReaderT Ctx m))))
  deriving (HasReader "fileName" Text) via Rename "_fileName" (Field "_fileName" () (MonadReader (ReaderT Ctx m)))

deriving instance MonadError e m => MonadError e (GriffT m)
deriving instance MTL.MonadState s m => MTL.MonadState s (GriffT m)

runGriffT :: MonadIO m => Text -> GriffT m a -> m a
runGriffT fileName (GriffT m) = do
  i <- liftIO $ newIORef 0
  runReaderT m $ Ctx { _uniq = i
                     , _fileName = fileName }
