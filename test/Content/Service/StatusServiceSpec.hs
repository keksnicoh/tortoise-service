{-# LANGUAGE OverloadedStrings #-}

module Content.Service.StatusServiceSpec (spec) where
import Content.Model.Status 
import Content.Service.StatusService
import Content.Model.StatusRequest
import Control.Monad.Identity
import Control.Exception
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
    it "should return the result of inner repository" $
      let 
        records =
          [ C.Status (read "550e8400-e29b-11d4-a716-446655440000") (Just 10) (Just 10) (Just 5) (Just 0) (read "2011-11-19 18:28:33")
          , C.Status (read "650e8400-e29b-11d4-a716-446655440000") Nothing (Just 12) Nothing (Just 6) (read "2011-11-20 18:28:42")
          ]
        service = mkGetStatusService (return records)
        result = runIdentity service
      in result `shouldBe` [from (head records), from (records !! 1)]

  describe "mkPostStatusService" $ do
    let
      uuid        = read "550e8400-e29b-11d4-a716-446655440000"
      created     = read "2011-11-20 18:28:45"
      env         = DummyEnvironment (return created) (return uuid)
      request     = StatusRequest (Just 11) (Just 12) (Just 5) (Just 0)
      expectedRow = C.Status uuid (Just 11) (Just 12) (Just 5) (Just 0) created
    it "should map a successfull insertion properly" $ do
      let
        insertStatusRepository = mockSingular (`shouldBe` expectedRow) C.Success
        service                = mkPostStatusService insertStatusRepository
      runReaderT (service request) env >>= flip shouldBe (from expectedRow)
    it "should throw an UUIDCollisionException on PkAlreadyExists summand" $ do
      let
        insertStatusRepository = mockSingular (`shouldBe` expectedRow) C.PkAlreadyExists
        service                = mkPostStatusService insertStatusRepository
      try (runReaderT (service request) env) >>= flip shouldBe (Left UUIDCollisionException)
