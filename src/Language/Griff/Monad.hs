{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
module Language.Griff.Monad where

import           Capability.Reader
import           Capability.State
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Data.IORef
import           Data.Text
import           GHC.Generics

data Ctx = Ctx { _uniq :: IORef Int
               , _fileName :: Text
               } deriving Generic

newtype GriffT m a = GriffT (ReaderT Ctx m a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "uniq" Int) via ReaderIORef (Rename "_uniq" (Field "_uniq" () (MonadReader (ReaderT Ctx m))))
  deriving (HasReader "fileName" Text) via Rename "_fileName" (Field "_fileName" () (MonadReader (ReaderT Ctx m)))

runGriffT :: MonadIO m => Text -> GriffT m a -> m a
runGriffT fileName (GriffT m) = do
  i <- liftIO $ newIORef 0
  runReaderT m $ Ctx { _uniq = i
                     , _fileName = fileName }
