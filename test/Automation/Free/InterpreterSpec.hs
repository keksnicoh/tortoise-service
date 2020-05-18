module Automation.Free.InterpreterSpec where

import           Test.Hspec

import qualified Core.Database.Model.Status    as CDMStatus

import           Automation.Header
import           Automation.Free.Interpreter
import qualified Automation.Free.SimpleController
                                               as S
import           Control.Monad.Writer           ( runWriter
                                                , tell
                                                , Writer
                                                )

spec :: Spec
spec = do
  describe "mkInterpreter" $ do
    let
      getLightStatus :: GetLightStatus (Writer [String])
      getLightStatus S.LightId1 = return S.LightOn
      getLightStatus S.LightId2 = return S.LightOff

      proposeSwitchLight :: ProposeSwitchLight (Writer [String])
      proposeSwitchLight lightId value =
        tell ["proposeLightSwitch " ++ show lightId ++ " " ++ show value]

      lockLight :: LockLight (Writer [String])
      lockLight lightId = tell ["lockLight " ++ show lightId]

      mockInterpreter status = mkInterpreter getLightStatus
                                             proposeSwitchLight
                                             lockLight
                                             (return status)

    it "getLightStatus returns corrent mocked data" $ do
      let interpreter = mockInterpreter []
          result      = runWriter . interpreter $ S.getLightStatus S.LightId1
      result `shouldBe` (S.LightOn, [])

    it "getTemperature returns nothing if status mock is empty" $ do
      let interpreter = mockInterpreter []
          result      = runWriter . interpreter $ S.getTemperature
      result `shouldBe` (Nothing, [])

    it "getTemperature returns head if of status mock" $ do
      let interpreter = mockInterpreter
            [ CDMStatus.Status { CDMStatus.statusId           = undefined
                               , CDMStatus.temperature        = Just 14
                               , CDMStatus.humidity           = undefined
                               , CDMStatus.temperatureOutside = undefined
                               , CDMStatus.humidityOutside    = undefined
                               , CDMStatus.created            = undefined
                               }
            ]
          result = runWriter . interpreter $ S.getTemperature
      result `shouldBe` (Just 14, [])

    it "proposeLightSwitch is invoked properly" $ do
      let interpreter = mockInterpreter []
          result      = runWriter . interpreter $ do
            S.proposeLightSwitch S.LightId1 True
            S.proposeLightSwitch S.LightId2 True
            S.proposeLightSwitch S.LightId1 False
            S.proposeLightSwitch S.LightId2 False
      result
        `shouldBe` ( ()
                   , [ "proposeLightSwitch LightId1 True"
                     , "proposeLightSwitch LightId2 True"
                     , "proposeLightSwitch LightId1 False"
                     , "proposeLightSwitch LightId2 False"
                     ]
                   )

    it "lockLight is invoked properly" $ do
      let interpreter = mockInterpreter []
          result      = runWriter . interpreter $ do
            S.lockLight S.LightId1
            S.lockLight S.LightId2
      result `shouldBe` ((), ["lockLight LightId1", "lockLight LightId2"])
