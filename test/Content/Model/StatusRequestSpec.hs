{-# LANGUAGE OverloadedStrings #-}

module Content.Model.StatusRequestSpec where

import           Test.Hspec
import           Content.Model.StatusRequest
import           Data.Aeson
import           GHC.Exts
import           Data.Aeson.Types               ( parse )
import           Data.UUID                     as UUID
import qualified Core.Database.Model.Status    as C

spec :: Spec
spec = do
  describe "FromJson" $ 
    it "should decode a json document properly" $ 
      let value = Object $ fromList 
            [ ("temperature", Number 2.1)
            , ("humidity", Number 10)
            ]
      in  parse parseJSON value `shouldBe` Success (StatusRequest 2.1 10)

  describe "toStatus" $ 
    it "should create a Database model" $ 
      let statusRequest = StatusRequest 13.37 42
          date = read "2019-08-31 05:14:37"
      in  toStatus statusRequest UUID.nil date `shouldBe` C.Status UUID.nil 13.37 42 date
