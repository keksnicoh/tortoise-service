{-# LANGUAGE OverloadedStrings #-}

module Content.Model.StatusSpec where

import Content.Model.Status (Status (Status), from)
import qualified Core.Database.Model.Status as C
import Data.Aeson
  ( ToJSON (toJSON),
    Value (Null, Number, Object, String),
  )
import qualified Data.UUID as UUID
import GHC.Exts (IsList (fromList))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "ToJson" $ do
    it "should convert Status into the expected Aeson Value" $
      let status =
            Status
              UUID.nil
              (Just 2.5)
              (Just 5)
              (Just 6)
              (Just 7)
              (read "2019-08-31 05:14:37Z")
          expectedValue =
            Object $
              fromList
                [ ("statusId", String "00000000-0000-0000-0000-000000000000"),
                  ("temperature", Number 2.5),
                  ("humidity", Number 5),
                  ("temperatureOutside", Number 6),
                  ("humidityOutside", Number 7),
                  ("created", String "2019-08-31T05:14:37Z")
                ]
       in toJSON status `shouldBe` expectedValue
    it "should convert a minimal Status into the expected Aeson Value" $
      let status =
            Status
              UUID.nil
              Nothing
              Nothing
              Nothing
              Nothing
              (read "2019-08-31 05:14:37Z")
          expectedValue =
            Object $
              fromList
                [ ("statusId", String "00000000-0000-0000-0000-000000000000"),
                  ("temperature", Null),
                  ("humidity", Null),
                  ("temperatureOutside", Null),
                  ("humidityOutside", Null),
                  ("created", String "2019-08-31T05:14:37Z")
                ]
       in toJSON status `shouldBe` expectedValue

  describe "from" $
    it "should read properties from core database model" $
      let coreStatus =
            C.Status
              UUID.nil
              (Just 13)
              (Just 1.4)
              Nothing
              (Just 9)
              (read "2019-08-31 05:14:37Z")
          expectedStatus =
            Status
              UUID.nil
              (Just 13)
              (Just 1.4)
              Nothing
              (Just 9)
              (read "2019-08-31 05:14:37Z")
       in from coreStatus `shouldBe` expectedStatus
