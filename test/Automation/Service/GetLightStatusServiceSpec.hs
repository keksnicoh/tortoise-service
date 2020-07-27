{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Service.GetLightStatusServiceSpec where

import           Test.Hspec

import           Control.Monad.Identity         ( runIdentity
                                                , Identity
                                                )
import           Control.Monad.Reader           ( runReaderT )
import qualified Data.Time                     as T
import qualified Core.State.Model.State        as CSMState

import           Automation.Service.GetLightStatusService
import           Automation.Model.SimpleHandlerConfig
import           Automation.Free.SimpleController
import OpenEnv

initialState :: CSMState.State
initialState = CSMState.initialState
  { CSMState.light1 = Nothing
  , CSMState.light2 = Nothing
  , CSMState.controlLockDate1 = Nothing
  , CSMState.controlLockDate2 = Nothing
  }
spec :: Spec
spec = do

  describe "mkGetLightStatus" $ do
    let
      config = SimpleHandlerConfig
        { l1TRange     = undefined
        , l2TRange     = undefined
        , lockDuration = 60
        }
    describe "at undefined light state" $ do
      it "map to LightUndefined if no lock-date is provided" $ do
        let stateRepository = return initialState
            env             = return @Identity (read "2019-02-03 13:37:42Z" :: T.UTCTime) #: config #: nil
            result lightId  = runIdentity $ runReaderT (mkGetLightStatus stateRepository lightId) env
        result LightId1 `shouldBe` LightUndefined
        result LightId2 `shouldBe` LightUndefined

      it "map to LightUndefined for lock-date lower, equal and greater than current-time" $ do
        let stateRepository = return $ initialState
              { CSMState.light1           = Nothing
              , CSMState.light2           = Nothing
              , CSMState.controlLockDate1 = Just (read "2019-02-03 13:36:41Z")
              , CSMState.controlLockDate2 = Just (read "2019-02-03 13:36:42Z")
              }
            lightService = mkGetLightStatus stateRepository
            runner time lightId =
              let env = return @Identity time #: config #: nil
              in  runIdentity $ runReaderT (lightService lightId) env

        -- both locked (ignore due to undefined)
        let run = runner (read "2019-02-03 13:37:41Z" :: T.UTCTime)
          in do
            run LightId1 `shouldBe` LightUndefined
            run LightId2 `shouldBe` LightUndefined

        -- light1 boundary (ignore due to undefined)
        let run = runner (read "2019-02-03 13:37:42Z" :: T.UTCTime)
          in do
            run LightId1 `shouldBe` LightUndefined
            run LightId2 `shouldBe` LightUndefined

        -- light2 boundary
        let run = runner (read "2019-02-03 13:37:43Z" :: T.UTCTime)
          in do
            run LightId1 `shouldBe` LightUndefined
            run LightId2 `shouldBe` LightUndefined

    describe "without a lock date" $ do
      it "map Manual light state properly" $ do
        let stateRepository = return initialState
              { CSMState.light1 = Just (CSMState.Manual True)
              , CSMState.light2 = Just (CSMState.Manual False)
              }
            env          = return @Identity (read "2019-02-03 13:37:42Z" :: T.UTCTime) #: config #: nil
            runner lightId = runIdentity $ runReaderT (mkGetLightStatus stateRepository lightId) env
        runner LightId1 `shouldBe` LightManual
        runner LightId2 `shouldBe` LightManual

      it "map Controlled light state properly" $ do
        let stateRepository = return initialState
              { CSMState.light1 = Just (CSMState.Controlled False)
              , CSMState.light2 = Just (CSMState.Controlled True)
              }
            lightService = mkGetLightStatus stateRepository
            env          = return @Identity (read "2019-02-03 13:37:42Z" :: T.UTCTime) #: config #: nil
            runner lightId = runIdentity $ runReaderT (lightService lightId) env
        runner LightId1 `shouldBe` LightOff
        runner LightId2 `shouldBe` LightOn

    describe "full defined state" $ do
      it "check whether both light boundaries are inclusive" $ do
        let stateRepository = return initialState
              { CSMState.light1           = Just (CSMState.Controlled False)
              , CSMState.light2           = Just (CSMState.Controlled True)
              , CSMState.controlLockDate1 = Just (read "2019-02-03 13:36:41Z")
              , CSMState.controlLockDate2 = Just (read "2019-02-03 13:36:42Z")
              }
            runner time lightId =
              let env = return @Identity @T.UTCTime time #: config #: nil
              in  runIdentity $ runReaderT (mkGetLightStatus stateRepository lightId) env

        -- both locked
        let run = runner (read "2019-02-03 13:37:41Z")
          in do
            run LightId1 `shouldBe` LightLocked
            run LightId2 `shouldBe` LightLocked

        -- light1 boundary
        let run = runner (read "2019-02-03 13:37:42Z")
          in do
            run LightId1 `shouldBe` LightOff
            run LightId2 `shouldBe` LightLocked

        -- light2 boundary
        let run = runner (read "2019-02-03 13:37:43Z")
          in do
            run LightId1 `shouldBe` LightOff
            run LightId2 `shouldBe` LightOn
