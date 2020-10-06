{-# LANGUAGE OverloadedStrings #-}

module Content.Model.StatusRequestSpec where

import Content.Model.StatusRequest
  ( StatusRequest (StatusRequest),
    toStatus,
  )
import qualified Core.Database.Model.Status as C
import Data.Aeson
  ( FromJSON (parseJSON),
    Result (Success),
    Value (Null, Number, Object),
  )
import Data.Aeson.Types (parse)
import Data.UUID as UUID (nil)
import GHC.Exts (IsList (fromList))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "FromJson" $ do
    it "should decode a json document properly" $
      let value =
            Object $
              fromList
                [ ("temperature", Number 2.1),
                  ("humidity", Number 10),
                  ("temperatureOutside", Number 3),
                  ("humidityOutside", Number 2)
                ]
       in parse parseJSON value
            `shouldBe` Success
              (StatusRequest (Just 2.1) (Just 10) (Just 3) (Just 2))
    it "should decode a minimal json document properly" $
      let value =
            Object $
              fromList
                [ ("temperature", Null),
                  ("humidity", Null),
                  ("temperatureOutside", Null),
                  ("humidityOutside", Null)
                ]
       in parse parseJSON value
            `shouldBe` Success
              (StatusRequest Nothing Nothing Nothing Nothing)

  describe "toStatus" $
    it "should create a Database model" $
      let statusRequest = StatusRequest (Just 13.37) (Just 42) (Just 4) (Just 8)
          date = read "2019-08-31 05:14:37Z"
       in toStatus statusRequest UUID.nil date
            `shouldBe` C.Status
              UUID.nil
              (Just 13.37)
              (Just 42)
              (Just 4)
              (Just 8)
              date
