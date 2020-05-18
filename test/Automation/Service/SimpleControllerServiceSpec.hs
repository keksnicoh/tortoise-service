{-# LANGUAGE LambdaCase #-}
module Automation.Service.SimpleControllerServiceSpec where

import           Test.Hspec
import           Control.Monad.Writer           ( runWriter
                                                , Writer
                                                , tell
                                                )

import           Core.Internal
import           Automation.Model.SimpleHandlerConfig
import           Automation.Service.SimpleControllerService
import           Automation.Free.SimpleController
import           Control.Monad.Free             ( foldFree )
import           Control.Monad.Reader           ( runReaderT
                                                , ReaderT
                                                )
import           Control.Monad                  ( forM_ )

type ST = ReaderT ControllerHandlerEnv (Writer [String])
newtype ControllerHandlerEnv = ControllerHandlerEnv SimpleHandlerConfig
instance HasSimpleHandlerConfig ControllerHandlerEnv where
  getSimpleHandlerConfig (ControllerHandlerEnv config) = config

spec :: Spec
spec = do
  describe "mkControllerHandler" $ do
    let mockInterpreter
          :: Maybe Temperature
          -> (LightId -> LightStatus)
          -> SimpleControllerInterpreter a ST
        mockInterpreter temperature getLightStatus = foldFree $ \case
          GetTemperature n -> n <$> do
            tell ["GetTemperature"]
            return temperature
          GetLightStatus lightId n -> n <$> do
            tell ["GetLightStatus " ++ show lightId]
            return (getLightStatus lightId)
          ProposeLightSwitch lightId s n ->
            n <$ tell ["ProposeLightSwitch " ++ show lightId ++ " " ++ show s]
          LockLight lightId n -> n <$ tell ["LockLight " ++ show lightId]

        runTest interpreter =
          let env = ControllerHandlerEnv $ SimpleHandlerConfig
                { l1TRange     = TRange 10 27
                , l2TRange     = TRange 9 28
                , lockDuration = 1337
                }
              controllerHandler = mkControllerHandler interpreter
          in  runWriter $ runReaderT controllerHandler env

    -- if there is no sensor data available, we exit the program
    it "should not perform any actions if temperature is undefined" $ do
      let interpreter = mockInterpreter Nothing (const LightUndefined)
      runTest interpreter `shouldBe` ((), ["GetTemperature"])

    -- check flow for LightUndefined states
    describe "undefined light states" $ do
      it "turn off both lights - without lock" $ do
        let interpreter = mockInterpreter (Just 28) (const LightUndefined)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "ProposeLightSwitch LightId1 False"
                       , "GetLightStatus LightId2"
                       , "ProposeLightSwitch LightId2 False"
                       ]
                     )
      it "turn off light1, turn on light2 - without lock" $ do
        let interpreter = mockInterpreter (Just 27) (const LightUndefined)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "ProposeLightSwitch LightId1 False"
                       , "GetLightStatus LightId2"
                       , "ProposeLightSwitch LightId2 True"
                       ]
                     )
      it "turn on both lights - without lock" $ do
        let interpreter = mockInterpreter (Just 26) (const LightUndefined)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "ProposeLightSwitch LightId1 True"
                       , "GetLightStatus LightId2"
                       , "ProposeLightSwitch LightId2 True"
                       ]
                     )
      it "turn on both lights (ignore lower bounds) - without lock" $ do
        let interpreter = mockInterpreter (Just 3) (const LightUndefined)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "ProposeLightSwitch LightId1 True"
                       , "GetLightStatus LightId2"
                       , "ProposeLightSwitch LightId2 True"
                       ]
                     )

    -- check flow for LightOff states
    describe "lowT boundary: lightOff" $ do
      it "should trigger proposition for both lights" $ do
        let interpreter = mockInterpreter (Just 8) (const LightOff)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "ProposeLightSwitch LightId1 True"
                       , "LockLight LightId1"
                       , "GetLightStatus LightId2"
                       , "ProposeLightSwitch LightId2 True"
                       , "LockLight LightId2"
                       ]
                     )
      it "should trigger proposition mockInterpreter light1" $ do
        let interpreter = mockInterpreter (Just 9) (const LightOff)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "ProposeLightSwitch LightId1 True"
                       , "LockLight LightId1"
                       , "GetLightStatus LightId2"
                       ]
                     )
      it "should not trigger any proposition" $ do
        let interpreter = mockInterpreter (Just 10) (const LightOff)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "GetLightStatus LightId2"
                       ]
                     )

    -- check flow for LightOn states
    describe "highT boundary: lightOn" $ do
      it "should trigger proposition for both lights" $ do
        let interpreter = mockInterpreter (Just 29) (const LightOn)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "ProposeLightSwitch LightId1 False"
                       , "LockLight LightId1"
                       , "GetLightStatus LightId2"
                       , "ProposeLightSwitch LightId2 False"
                       , "LockLight LightId2"
                       ]
                     )
      it "should trigger proposition for light1" $ do
        let interpreter = mockInterpreter (Just 28) (const LightOn)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "ProposeLightSwitch LightId1 False"
                       , "LockLight LightId1"
                       , "GetLightStatus LightId2"
                       ]
                     )
      it "should not trigger any proposition" $ do
        let interpreter = mockInterpreter (Just 27) (const LightOn)
        runTest interpreter
          `shouldBe` ( ()
                     , [ "GetTemperature"
                       , "GetLightStatus LightId1"
                       , "GetLightStatus LightId2"
                       ]
                     )
    -- locked and manual light state should be ignored
    forM_ [("LightLocked", LightLocked), ("LightManual", LightManual)]
      $ \(label, status) -> do
          let result =
                ( ()
                , [ "GetTemperature"
                  , "GetLightStatus LightId1"
                  , "GetLightStatus LightId2"
                  ]
                )
          describe ("no actions should be performed for: " ++ label) $ do
            it "lowT should be ignored" $ do
              let interpreter = mockInterpreter (Just 0) (const status)
              runTest interpreter `shouldBe` result
            it "habitable T should be ignored" $ do
              let interpreter = mockInterpreter (Just 19) (const status)
              runTest interpreter `shouldBe` result
            it "highT should be ignored" $ do
              let interpreter = mockInterpreter (Just 50) (const status)
              runTest interpreter `shouldBe` result

    -- ensure that lightids are passed properly
    it "mixed states should be recognized correctly" $ do
      let status LightId1 = LightOff
          status LightId2 = LightLocked
          interpreter = mockInterpreter (Just 0) status
      runTest interpreter
        `shouldBe` ( ()
                   , [ "GetTemperature"
                     , "GetLightStatus LightId1"
                     , "ProposeLightSwitch LightId1 True"
                     , "LockLight LightId1"
                     , "GetLightStatus LightId2"
                     ]
                   )
