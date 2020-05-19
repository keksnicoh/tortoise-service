{-# LANGUAGE LambdaCase #-}
module Automation.Free.SimpleControllerSpec where

import           Control.Monad.Free
import           Control.Monad.Writer           ( runWriter
                                                , Writer
                                                , tell
                                                )

import           Test.Hspec

import           Automation.Free.SimpleController

data LogInterpreter
  = LogGetLightTemperature
  | LogGetLightStatus LightId
  | LogProposeLightSwitch LightId Bool
  | LogLockLight LightId
  deriving (Eq, Show)

spec :: Spec
spec = do
  let interpreter :: Free SimpleController a -> Writer [LogInterpreter] a
      interpreter = foldFree $ \case
        GetTemperature next ->
          next <$> (tell [LogGetLightTemperature] >> return (Just 5))
        LockLight lightId next -> next <$ tell [LogLockLight lightId]
        GetLightStatus lightId next ->
          next <$> (tell [LogGetLightStatus lightId] >> return LightOn)
        ProposeLightSwitch lightId v next ->
          next <$ tell [LogProposeLightSwitch lightId v]

  it "getTemperature" $ do
    runWriter (interpreter getTemperature)
      `shouldBe` (Just 5, [LogGetLightTemperature])
  it "getLightStatus" $ do
    runWriter (interpreter (getLightStatus LightId1))
      `shouldBe` (LightOn, [LogGetLightStatus LightId1])
  it "proposeLightSwitch" $ do
    runWriter (interpreter (proposeLightSwitch LightId2 False))
      `shouldBe` ((), [LogProposeLightSwitch LightId2 False])
  it "lockLight" $ do
    runWriter (interpreter (lockLight LightId2))
      `shouldBe` ((), [LogLockLight LightId2])
