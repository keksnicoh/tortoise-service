{-# LANGUAGE OverloadedStrings #-}

module Content.Model.StatusSpec where

import           Test.Hspec
import           Content.Model.Status
import qualified Core.Database.Model.Status    as C
import           GHC.Exts
import qualified Data.UUID                     as UUID
import           Data.Aeson

spec :: Spec
spec = do
  describe "ToJson" $ 
    it "should convert Status into the expected Aeson Value" $ 
      let status        = Status UUID.nil 2.5 5 (read "2019-08-31 05:14:37")
          expectedValue = Object $ fromList
            [ ("statusId",    String "00000000-0000-0000-0000-000000000000")
            , ("temperature", Number 2.5)
            , ("humidity",    Number 5)
            , ("created",     String "2019-08-31T05:14:37Z")
            ]
      in  toJSON status `shouldBe` expectedValue

  describe "from" $
    it "should read properties from core database model" $ 
      let coreStatus     = C.Status UUID.nil 13 1.4 (read "2019-08-31 05:14:37")
          expectedStatus = Status UUID.nil 13 1.4 (read "2019-08-31 05:14:37")
      in  from coreStatus `shouldBe` expectedStatus
