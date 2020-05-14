{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.HouseStateSpec where

import           TestUtil
import           Test.Hspec
import           Automation.HouseState
import qualified Data.Time                     as T
import qualified Dependencies                  as D
import           Control.Monad.Reader           ( liftIO
                                                , forM_
                                                , ReaderT(runReaderT)
                                                )
import qualified Core.Database.Model.Status    as CDMStatus
import qualified Core.State.Model.State        as CSMState
import           Core.FSM
import           Data.IORef                     ( newIORef
                                                , modifyIORef'
                                                , readIORef
                                                )
import           Automation.Config

type RT = ReaderT MkReadSensorEnv IO

data MkReadSensorEnv
  = MkReadSensorEnv
    HouseStateConfig
    (IO T.UTCTime)

instance HasHouseStateConfig MkReadSensorEnv where
  getHouseStateConfig (MkReadSensorEnv c _) = c

instance D.HasCurrentTime MkReadSensorEnv RT where
  getCurrentTime (MkReadSensorEnv _ t) = liftIO t

uuid = read "550e8400-e29b-11d4-a716-446655440000"

status = CDMStatus.Status { CDMStatus.statusId           = uuid
                          , CDMStatus.temperature        = Nothing
                          , CDMStatus.humidity           = Nothing
                          , CDMStatus.temperatureOutside = Nothing
                          , CDMStatus.humidityOutside    = Nothing
                          , CDMStatus.created = read "2019-09-04 13:36:32"
                          }

undefinedConfig = HouseStateConfig { delaySensorRead = undefined
                                   , minTemperature  = undefined
                                   , maxTemperature  = undefined
                                   , retrySensorRead = undefined
                                   , maxStatusAge    = undefined
                                   , emergencyDelay  = undefined
                                   }
spec :: Spec
spec = do
  describe "mkReadSensor" $ do
    let config = HouseStateConfig { delaySensorRead = undefined
                                  , minTemperature  = 10
                                  , maxTemperature  = 40
                                  , retrySensorRead = 0
                                  , maxStatusAge    = 60
                                  , emergencyDelay  = return ()
                                  }
        now            = read "2019-09-04 13:37:32"
        getCurrentTime = return now

    it "should return Nothing when no status was provided" $ do
      let mockedRepository = return []
          readerSensor     = mkReadSensor mockedRepository
          env              = MkReadSensorEnv config getCurrentTime
      runReaderT readerSensor env >>?= Nothing

    it "should return status which are not older than maxStatusAge" $ do
      let mockedRepository = return
            [ status { CDMStatus.created     = read "2019-09-04 13:36:15"
                     , CDMStatus.temperature = Nothing
                     }
            , status { CDMStatus.created     = read "2019-09-04 13:36:32"
                     , CDMStatus.temperature = Just 5
                     }
            ]
          readerSensor = mkReadSensor mockedRepository
      let env = MkReadSensorEnv (config { maxStatusAge = 59 }) getCurrentTime
      runReaderT readerSensor env >>?= Nothing
      let env = MkReadSensorEnv (config { maxStatusAge = 60 }) getCurrentTime
      runReaderT readerSensor env >>?= Just (Low 5)

    describe "should interpret config (minT, maxT) boundaries inclusively" $ do
      forM_
          [ ("Lower Bound"          , (5.001, 6) , Low 5)
          , ("Lower Bound Inclusive", (5, 6)     , Bound 5)
          , ("Bound"                , (4.99, 6)  , Bound 5)
          , ("Upper Bound Inclusive", (4.5, 5)   , Bound 5)
          , ("Upper Bound"          , (4.5, 4.99), High 5)
          ]
        $ \(label, range, expectedResult) -> it label $ do
            let mockedRepository =
                  return
                    [ status { CDMStatus.created     = now
                             , CDMStatus.temperature = Just 5
                             }
                    ]
                env (minT, maxT) = MkReadSensorEnv
                  (config { minTemperature = minT, maxTemperature = maxT })
                  getCurrentTime
                readerSensor = mkReadSensor mockedRepository

            runReaderT readerSensor (env range) >>?= Just expectedResult

  describe "HouseState#Show"
    $ forM_
        [ (show Initializing              , "Initializing")
        , (show (HasSensorData $ Bound 12), "HasSensorData Bound 12.0")
        , (show TemperatureBound          , "TemperatureBound")
        , (show RetrySensor               , "RetrySensor")
        , (show Emergency                 , "Emergency")
        , ( show (Terminating OutOfControlReason)
          , "Terminating OutOfControlReason"
          )
        ]
    $ \(subj, expected) -> it ("render " ++ expected) $ subj `shouldBe` expected

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
          env = MkReadSensorEnv config undefined
        return (logIORef, service, env)

      delayAction _ = return ()
      jmFalse = Just (CSMState.Manual False)
      jmTrue  = Just (CSMState.Manual True)
      jcFalse = Just (CSMState.Controlled False)
      jcTrue  = Just (CSMState.Controlled True)

    describe "high temperature lead to turning off the lights" $ do
      it "overwrite undefined lights and keep after delay" $ do
        (ioRef, service, env) <- mock delayAction CSMState.initialState
        runReaderT (service (High 5)) env >>= flip shouldBe ()
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
        readIORef ioRef >>= flip shouldBe expectedResult

      it "overwrite light1 (manual) and reset to original" $ do
        let initial = CSMState.initialState { CSMState.light1 = jmFalse }
        (ioRef, service, env) <- mock delayAction initial
        runReaderT (service (High 5)) env >>= flip shouldBe ()

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

        readIORef ioRef >>= flip shouldBe expectedResult

      it "overwrite light2 (manual) and reset to original" $ do
        let initial = CSMState.initialState { CSMState.light2 = jmFalse }
        (ioRef, service, env) <- mock delayAction initial
        runReaderT (service (High 5)) env >>= flip shouldBe ()

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
        readIORef ioRef >>= flip shouldBe expectedResult

      it "new manual light state during delay should not be changed" $ do
        let initial = CSMState.initialState { CSMState.light2 = jmTrue }
            mod logIORef = do
              modifyIORef' logIORef $ \s ->
                s { mockState = (mockState s) { CSMState.light1 = jmTrue } }
        (ioRef, service, env) <- mock mod initial
        runReaderT (service (High 5)) env >>= flip shouldBe ()

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
        readIORef ioRef >>= flip shouldBe expectedResult


    describe "low temperature lead to turning on the lights" $ do
      it "overwrite undefined lights and keep after delay" $ do
        (ioRef, service, env) <- mock delayAction CSMState.initialState
        runReaderT (service (Low 5)) env >>= flip shouldBe ()
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
        readIORef ioRef >>= flip shouldBe expectedResult

      it "overwrite light1 (manual) and reset to original" $ do
        let initial = CSMState.initialState { CSMState.light1 = jmTrue }
        (ioRef, service, env) <- mock delayAction initial
        runReaderT (service (Low 5)) env >>= flip shouldBe ()

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

        readIORef ioRef >>= flip shouldBe expectedResult

      it "new manual light state during delay should not be changed" $ do
        let
          initial = CSMState.initialState { CSMState.light1 = jmTrue }
          mod logIORef = do
            modifyIORef' logIORef $ \s ->
              s { mockState = (mockState s) { CSMState.light2 = jmFalse } }
        (ioRef, service, env) <- mock mod initial
        runReaderT (service (Low 5)) env >>= flip shouldBe ()

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
        readIORef ioRef >>= flip shouldBe expectedResult

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
