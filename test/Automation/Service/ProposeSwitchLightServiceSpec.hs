{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Service.ProposeSwitchLightServiceSpec where

import           Test.Hspec

import qualified Core.State.Model.State        as CSMState
import qualified Core.State.Repository.State   as CSRState

import           Automation.Service.ProposeSwitchLightService
import           Automation.Free.SimpleController
import           Automation.Header
import           Control.Monad                  ( forM_ )
import           Control.Monad.Writer           ( tell
                                                , runWriter
                                                , Writer
                                                )

initialState :: CSMState.State
initialState = CSMState.initialState { CSMState.light1           = Nothing
                                     , CSMState.light2           = Nothing
                                     , CSMState.controlLockDate1 = Nothing
                                     , CSMState.controlLockDate2 = Nothing
                                     }

spec :: Spec
spec = do

  describe "mkProposeLightSwitch" $ do
    let
      runService lightStatus = do
        let
          getLightStatus
            :: GetLightStatus (Writer [CSMState.State -> CSMState.State])
          getLightStatus LightId1 = return lightStatus
          getLightStatus LightId2 = return lightStatus

          updateState
            :: CSRState.UpdateState (Writer [CSMState.State -> CSMState.State])
          updateState statef = tell [statef]

          service
            :: ProposeSwitchLight (Writer [CSMState.State -> CSMState.State])
          service       = mkProposeSwitchLight getLightStatus updateState

          (result, log) = runWriter $ do
            service LightId1 True
            service LightId1 False
            service LightId2 True
            service LightId2 False
          mappedStates = (\f -> f initialState) <$> log
        mappedStates

    forM_
        [ ("LightUndefined", LightUndefined)
        , ("LightOn"       , LightOn)
        , ("LightOff"      , LightOff)
        ]
      $ \(label, lightStatus) ->
          it ("overwrite state by proposed values for: " ++ label) $ do
            runService lightStatus
              `shouldBe` [ initialState
                           { CSMState.light1 = Just (CSMState.Controlled True)
                           }
                         , initialState
                           { CSMState.light1 = Just (CSMState.Controlled False)
                           }
                         , initialState
                           { CSMState.light2 = Just (CSMState.Controlled True)
                           }
                         , initialState
                           { CSMState.light2 = Just (CSMState.Controlled False)
                           }
                         ]

    forM_ [("LightLocked", LightLocked), ("LightManual", LightManual)]
      $ \(label, lightStatus) -> it ("ignore proposition for: " ++ label) $ do
          runService lightStatus `shouldBe` []
