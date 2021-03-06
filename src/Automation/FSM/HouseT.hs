{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Automation.FSM.HouseT
  ( HouseT(..)
  )
where

import           Automation.FSM.HouseFSM     (HouseFSM (..))
import           Automation.Model.HouseState (HouseState (Emergency, HasSensorData, Initializing, RetrySensor, TemperatureBound),
                                              reasonFrom)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader        (MonadReader, ReaderT (..))
import qualified Core.State.Model.State      as CSMState
import           Data.IORef                  (IORef, modifyIORef')
import           HList                       (Get (..))
import           OpenEnv                     (EmbeddedF (..), HList, Provides,
                                              provide)

-- |MTL style transformer
newtype HouseT m a = HouseT
  { runHouseT :: m a
  } deriving ( Functor
             , Monad
             , Applicative
             , MonadIO
             , MonadReader e
             )

instance (Traversable t, Applicative m, Get t (m a) ts)
  => EmbeddedF t a (HList ts) (HouseT (ReaderT (HList ts) m)) where
  embeddedFromF = HouseT . ReaderT . const . sequenceA . getF

instance (MonadIO m, MonadReader e m, Provides (IORef CSMState.State) e)
  => HouseFSM (HouseT m) where
  type State (HouseT m) = HouseState

  initialize = do
    monitor CSMState.MonitorOff
    liftIO $ putStrLn "initialize house monitoring"
    return Initializing

  sensorRead sd _ = do
    monitor CSMState.MonitorOK
    liftIO $ putStrLn ("house monitoring: received sensor data - " ++ show sd)
    return (HasSensorData sd)

  verified _ = do
    monitor CSMState.MonitorOK
    liftIO $ putStrLn "house monitoring: temperature verified"
    return TemperatureBound

  retry _ = do
    monitor CSMState.MonitorSensorRetry
    liftIO $ putStrLn "house monitoring: could not read sensor. retry..."
    return RetrySensor

  emergency _ = do
    monitor CSMState.MonitorEmergency
    liftIO $ putStrLn "house monitoring: temperature not bound, emergency"
    return Emergency

  terminate event = do
    monitor CSMState.MonitorOff
    liftIO $ putStrLn "house monitoring: terminating"
    return $ reasonFrom event

monitor
  :: (MonadReader e m, Provides (IORef CSMState.State) e, MonadIO m)
  => CSMState.HouseMonitor
  -> m ()
monitor m = do
  stateIORef <- provide
  liftIO $ modifyIORef' stateIORef $ \s -> s { CSMState.houseMonitor = m }
