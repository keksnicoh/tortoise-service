{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Service.EmergencyServiceSpec where

import           TestUtil
import           Test.Hspec
import           Control.Monad.Reader           ( liftIO
                                                , ReaderT(runReaderT)
                                                )
import qualified Core.State.Model.State        as CSMState
import           Automation.FSM.HouseFSM
import           Data.IORef                     ( newIORef
                                                , modifyIORef'
                                                , readIORef
                                                )
import           Automation.Model.HouseStateConfig
import           Automation.Service.EmergencyService
import           OpenEnv


undefinedConfig :: HouseStateConfig
undefinedConfig = HouseStateConfig { delaySensorRead = undefined
                                   , minTemperature  = undefined
                                   , maxTemperature  = undefined
                                   , maxStatusAge    = undefined
                                   , emergencyDelay  = undefined
                                   }
spec :: Spec
spec = do
  describe "mkEmergencyAction" $ do
    let
      mock delayf state = do
        logIORef <- newIORef $ MkEmergencyActionMock [] state
        let
          log v =
            modifyIORef' logIORef $ \s -> s { mockLog = mockLog s ++ [v] }
          getStatus = liftIO $ do
            log GetStatusLog
            mockState <$> readIORef logIORef
          setStatus f = liftIO $ do
            newState <- f . mockState <$> readIORef logIORef
            modifyIORef' logIORef $ \s -> s { mockState = newState }
            log (SetStatusLog newState)
          service = mkEmergencyAction getStatus setStatus
          config  = undefinedConfig
            { emergencyDelay = log DelayLog >> delayf logIORef
            }
          env = config #: nil
        return (logIORef, service, env)

      delayAction _ = return ()
      jmFalse = Just (CSMState.Manual False)
      jmTrue  = Just (CSMState.Manual True)
      jcFalse = Just (CSMState.Controlled False)
      jcTrue  = Just (CSMState.Controlled True)

    describe "high temperature lead to turning off the lights" $ do
      it "overwrite undefined lights and keep after delay" $ do
        (ioRef, service, env) <- mock delayAction CSMState.initialState
        runReaderT (service (High 5)) env >>?= ()
        let expectedState = CSMState.initialState { CSMState.light1 = jcFalse
                                                  , CSMState.light2 = jcFalse
                                                  }
            expectedResult = MkEmergencyActionMock
              { mockLog   = [ GetStatusLog
                            , SetStatusLog expectedState
                            , DelayLog
                            , GetStatusLog
                            ]
              , mockState = expectedState
              }
        readIORef ioRef >>?= expectedResult

      it "overwrite light1 (manual) and reset to original" $ do
        let initial = CSMState.initialState { CSMState.light1 = jmFalse }
        (ioRef, service, env) <- mock delayAction initial
        runReaderT (service (High 5)) env >>?= ()

        let expectedEndState = CSMState.initialState
              { CSMState.light1 = jmFalse
              , CSMState.light2 = jcFalse
              }
            expectedResult = MkEmergencyActionMock
              { mockLog   = [ GetStatusLog
                            , SetStatusLog CSMState.initialState
                              { CSMState.light1 = jcFalse
                              , CSMState.light2 = jcFalse
                              }
                            , DelayLog
                            , GetStatusLog
                            , SetStatusLog expectedEndState
                            ]
              , mockState = expectedEndState
              }

        readIORef ioRef >>?= expectedResult

      it "overwrite light2 (manual) and reset to original" $ do
        let initial = CSMState.initialState { CSMState.light2 = jmFalse }
        (ioRef, service, env) <- mock delayAction initial
        runReaderT (service (High 5)) env >>?= ()

        let expectedEndState = CSMState.initialState
              { CSMState.light1 = jcFalse
              , CSMState.light2 = jmFalse
              }
            expectedResult = MkEmergencyActionMock
              { mockLog   = [ GetStatusLog
                            , SetStatusLog CSMState.initialState
                              { CSMState.light1 = jcFalse
                              , CSMState.light2 = jcFalse
                              }
                            , DelayLog
                            , GetStatusLog
                            , SetStatusLog expectedEndState
                            ]
              , mockState = expectedEndState
              }
        readIORef ioRef >>?= expectedResult

      it "new manual light state during delay should not be changed" $ do
        let initial = CSMState.initialState { CSMState.light2 = jmTrue }
            mod logIORef = do
              modifyIORef' logIORef $ \s ->
                s { mockState = (mockState s) { CSMState.light1 = jmTrue } }
        (ioRef, service, env) <- mock mod initial
        runReaderT (service (High 5)) env >>?= ()

        let expectedEndState = CSMState.initialState { CSMState.light1 = jmTrue
                                                     , CSMState.light2 = jmTrue
                                                     }
            expectedResult = MkEmergencyActionMock
              { mockLog   = [ GetStatusLog
                            , SetStatusLog CSMState.initialState
                              { CSMState.light1 = jcFalse
                              , CSMState.light2 = jcFalse
                              }
                            , DelayLog
                            , GetStatusLog
                            , SetStatusLog expectedEndState
                            ]
              , mockState = expectedEndState
              }
        readIORef ioRef >>?= expectedResult


    describe "low temperature lead to turning on the lights" $ do
      it "overwrite undefined lights and keep after delay" $ do
        (ioRef, service, env) <- mock delayAction CSMState.initialState
        runReaderT (service (Low 5)) env >>?= ()
        let expectedState = CSMState.initialState { CSMState.light1 = jcTrue
                                                  , CSMState.light2 = jcTrue
                                                  }
            expectedResult = MkEmergencyActionMock
              { mockLog   = [ GetStatusLog
                            , SetStatusLog expectedState
                            , DelayLog
                            , GetStatusLog
                            ]
              , mockState = expectedState
              }
        readIORef ioRef >>?= expectedResult

      it "overwrite light1 (manual) and reset to original" $ do
        let initial = CSMState.initialState { CSMState.light1 = jmTrue }
        (ioRef, service, env) <- mock delayAction initial
        runReaderT (service (Low 5)) env >>?= ()

        let expectedEndState = CSMState.initialState { CSMState.light1 = jmTrue
                                                     , CSMState.light2 = jcTrue
                                                     }
            expectedResult = MkEmergencyActionMock
              { mockLog   = [ GetStatusLog
                            , SetStatusLog CSMState.initialState
                              { CSMState.light1 = jcTrue
                              , CSMState.light2 = jcTrue
                              }
                            , DelayLog
                            , GetStatusLog
                            , SetStatusLog expectedEndState
                            ]
              , mockState = expectedEndState
              }

        readIORef ioRef >>?= expectedResult

      it "new manual light state during delay should not be changed" $ do
        let
          initial = CSMState.initialState { CSMState.light1 = jmTrue }
          mod logIORef = do
            modifyIORef' logIORef $ \s ->
              s { mockState = (mockState s) { CSMState.light2 = jmFalse } }
        (ioRef, service, env) <- mock mod initial
        runReaderT (service (Low 5)) env >>?= ()

        let expectedEndState = CSMState.initialState
              { CSMState.light1 = jmTrue
              , CSMState.light2 = jmFalse
              }
            expectedResult = MkEmergencyActionMock
              { mockLog   = [ GetStatusLog
                            , SetStatusLog CSMState.initialState
                              { CSMState.light1 = jcTrue
                              , CSMState.light2 = jcTrue
                              }
                            , DelayLog
                            , GetStatusLog
                            , SetStatusLog expectedEndState
                            ]
              , mockState = expectedEndState
              }
        readIORef ioRef >>?= expectedResult

data MkEmergencyActionMock
  = MkEmergencyActionMock
  { mockLog :: [LogMkEmergencyAction]
  , mockState :: CSMState.State
  }
  deriving (Show, Eq)

data LogMkEmergencyAction
  = DelayLog
  | GetStatusLog
  | SetStatusLog CSMState.State
  deriving (Eq, Show)
