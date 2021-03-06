{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Service.ReadSensorServiceSpec where

import           Test.Hspec
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import qualified Core.Database.Model.Status    as CDMStatus
import           Automation.Model.HouseStateConfig
import           Automation.FSM.HouseFSM
import           Automation.Service.ReadSensorService
import           Control.Monad.Identity         (Identity,  runIdentity )
import           Control.Monad                  ( forM_ )
import           OpenEnv

status :: CDMStatus.Status
status = CDMStatus.Status
  { CDMStatus.statusId           = read "550e8400-e29b-11d4-a716-446655440000"
  , CDMStatus.temperature        = Nothing
  , CDMStatus.humidity           = Nothing
  , CDMStatus.temperatureOutside = Nothing
  , CDMStatus.humidityOutside    = Nothing
  , CDMStatus.created            = read "2019-09-04 13:36:32Z"
  }

spec :: Spec
spec = do
  describe "mkReadSensor" $ do
    let config = HouseStateConfig { delaySensorRead = undefined
                                  , minTemperature  = 10
                                  , maxTemperature  = 40
                                  , maxStatusAge    = 60
                                  , emergencyDelay  = return ()
                                  }
        now            = read "2019-09-04 13:37:32Z"
        getCurrentTime = return @Identity now

    it "should return Nothing when no status was provided" $ do
      let mockedRepository = return []
          readerSensor     = mkReadSensor mockedRepository
          env              = config #: getCurrentTime #: nil
          result           = runReaderT readerSensor env
      runIdentity result `shouldBe` Nothing

    it "should return status which are not older than maxStatusAge" $ do
      let mockedRepository = return
            [ status { CDMStatus.created     = read "2019-09-04 13:36:15Z"
                     , CDMStatus.temperature = Nothing
                     }
            , status { CDMStatus.created     = read "2019-09-04 13:36:32Z"
                     , CDMStatus.temperature = Just 5
                     }
            ]
          readerSensor = mkReadSensor mockedRepository
          runner env = runIdentity $ runReaderT readerSensor env

      -- test threshold
      let env = config { maxStatusAge = 59 } #: getCurrentTime #: nil
      runner env `shouldBe` Nothing

      -- test boundary
      let env = config { maxStatusAge = 60 } #: getCurrentTime #: nil
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
                env (minT, maxT) = (config { minTemperature = minT, maxTemperature = maxT })
                  #: getCurrentTime #: nil
                readerSensor = mkReadSensor mockedRepository
                result       = runReaderT readerSensor (env range)
            runIdentity result `shouldBe` Just expectedResult
