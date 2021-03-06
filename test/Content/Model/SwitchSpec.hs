{-# LANGUAGE OverloadedStrings #-}

module Content.Model.SwitchSpec where

import Content.Model.Switch
  ( Switch (Switch),
    SwitchRequest (SwitchRequest),
    fromCoreSwitch,
    toCoreSwitch,
  )
import qualified Core.State.Model.State as C
import Data.Aeson
  ( FromJSON (parseJSON),
    Result (Success),
    ToJSON (toJSON),
    Value (Bool, Object),
    eitherDecode,
  )
import Data.Aeson.Types (parse)
import qualified Data.ByteString.Lazy as BSL
import GHC.Exts (IsList (fromList))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Switch#FromJSON" $
    it "must decode json properly" $
      let value =
            Object $
              fromList
                [ ("value", Bool True),
                  ("controlled", Bool False)
                ]
          expectedModel = Switch True False
       in parse parseJSON value `shouldBe` Success expectedModel

  describe "Switch#ToJSON" $
    it "encode json properly" $
      let switch = Switch False True
          expectedValue =
            Object $
              fromList
                [ ("value", Bool False),
                  ("controlled", Bool True)
                ]
       in toJSON switch `shouldBe` expectedValue

  describe "SwitchRequest#FromJSON" $
    it "must decode json properly" $ do
      let expectedModel =
            SwitchRequest
              (Just $ Switch False True)
              (Just $ Switch True False)
      contents <- BSL.readFile "test/fixtures/content/switchRequest0.json"
      eitherDecode contents `shouldBe` Right expectedModel

  describe "toCoreSwitch" $ do
    it "must transform properly - Controlled" $
      toCoreSwitch (Switch True False) `shouldBe` C.Manual True
    it "must transform properly - Manual" $
      toCoreSwitch (Switch False True) `shouldBe` C.Controlled False

  describe "fromCoreSwitch" $ do
    it "must transform properly - Controlled" $
      fromCoreSwitch (C.Manual True) `shouldBe` Switch True False
    it "must transform properly - Manual" $
      fromCoreSwitch (C.Controlled False) `shouldBe` Switch False True