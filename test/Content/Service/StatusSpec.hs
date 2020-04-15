{-# LANGUAGE OverloadedStrings #-}

module Content.Service.StatusSpec (spec) where
import Content.Model.Status 
import Content.Service.Status
import Content.Model.StatusRequest
import Control.Monad.Identity
import qualified Core.Database.Model.Status as C
import Test.Hspec
import qualified Data.UUID as U
import qualified Data.Time as T
import Dependencies
import Control.Monad.Reader

import TestUtil

data DummyEnvironment = DummyEnvironment (IO T.UTCTime) (IO U.UUID)
instance HasCurrentTime DummyEnvironment where getCurrentTime (DummyEnvironment t _) = t 
instance HasRandomUUID DummyEnvironment where  getRandomUUID (DummyEnvironment _ u) = u

spec :: Spec
spec = do
  describe "mkGetStatusService" $ 
    it "should derp" $
      let 
        records =
          [ C.Status (read "550e8400-e29b-11d4-a716-446655440000") 10 10 (read "2011-11-19 18:28:33")
          , C.Status (read "650e8400-e29b-11d4-a716-446655440000") 11 12 (read "2011-11-20 18:28:42")
          ]
        mockedService = mkGetStatusService (return records)
        result = runIdentity mockedService
      in result `shouldBe` [from (head records), from (records !! 1)]

  describe "mkPostStatusService" $ do
    let
      uuid        = read "550e8400-e29b-11d4-a716-446655440000"
      created     = read "2011-11-20 18:28:45"
      env         = DummyEnvironment (return created) (return uuid)
      request     = StatusRequest 11 12
      expectedRow = C.Status uuid 11 12 created

    it "should work like a charm omg" $ do
      let
        insertStatusRepository = mockSingular (`shouldBe` expectedRow) C.Success
        service                = mkPostStatusService insertStatusRepository
      runReaderT (service request) env >>= shouldBe (from expectedRow)
