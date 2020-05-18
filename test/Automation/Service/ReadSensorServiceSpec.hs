{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Service.ReadSensorServiceSpec where

import           Test.Hspec
import qualified Data.Time                     as T
import qualified Dependencies                  as D
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import qualified Core.Database.Model.Status    as CDMStatus
import           Automation.Model.HouseStateConfig
import           Automation.FSM.HouseFSM
import           Automation.Service.ReadSensorService
import           Control.Monad.Identity         ( runIdentity
                                                , Identity
                                                )
import           Control.Monad                  ( forM_ )

type RT = ReaderT MkReadSensorEnv Identity

data MkReadSensorEnv
  = MkReadSensorEnv HouseStateConfig (Identity T.UTCTime)

instance HasHouseStateConfig MkReadSensorEnv where
  getHouseStateConfig (MkReadSensorEnv c _) = c

instance D.HasCurrentTime MkReadSensorEnv RT where
  getCurrentTime (MkReadSensorEnv _ t) = return (runIdentity t)

status :: CDMStatus.Status
status = CDMStatus.Status
  { CDMStatus.statusId           = read "550e8400-e29b-11d4-a716-446655440000"
  , CDMStatus.temperature        = Nothing
  , CDMStatus.humidity           = Nothing
  , CDMStatus.temperatureOutside = Nothing
  , CDMStatus.humidityOutside    = Nothing
  , CDMStatus.created            = read "2019-09-04 13:36:32"
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
          result           = runReaderT readerSensor env
      runIdentity result `shouldBe` Nothing

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
          runner env = runIdentity $ runReaderT readerSensor env

      -- test threshold
      let env    = MkReadSensorEnv (config { maxStatusAge = 59 }) getCurrentTime
      runner env `shouldBe` Nothing

      -- test boundary
      let env = MkReadSensorEnv (config { maxStatusAge = 60 }) getCurrentTime
      runner env `shouldBe` Just (Low 5)

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
                result       = runReaderT readerSensor (env range)
            runIdentity result `shouldBe` Just expectedResult
