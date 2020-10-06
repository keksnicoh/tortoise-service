{-# LANGUAGE FlexibleContexts #-}
module Core.State.Repository.State
  ( UpdateState
  , updateState
  , GetState
  , currentState
  )
where

import           Control.Monad.Reader   (MonadIO, MonadReader, liftIO)
import           Core.State.Model.State (State)
import           Data.IORef             (IORef, atomicModifyIORef', readIORef)
import           OpenEnv                (Provides, provide)

type UpdateState m = (State -> State) -> m ()
type GetState m = m State

updateState
  :: (MonadIO m, MonadReader e m, Provides (IORef State) e) => UpdateState m
updateState modify = do
  ioRef <- provide
  liftIO $ atomicModifyIORef' ioRef $ \state -> (modify state, ())

currentState
  :: (MonadIO m, MonadReader e m, Provides (IORef State) e) => GetState m
currentState = provide >>= liftIO . readIORef
