{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Content.Service.StatusServiceSpec
  ( spec
  )
where
import           Content.Model.Status
import           Content.Service.StatusService
import           Content.Model.StatusRequest
import           Control.Monad.Identity
import qualified Core.Database.Model.Status    as C
import           Test.Hspec
import           Control.Monad.Reader
import Control.Monad.Catch.Pure
import OpenEnv

spec :: Spec
spec = do
  describe "mkGetStatusService" $ do
    it "should return the result of inner repository" $ do
      let records =
            [ C.Status (read "550e8400-e29b-11d4-a716-446655440000")
                       (Just 10)
                       (Just 10)
                       (Just 5)
                       (Just 0)
                       (read "2011-11-19 18:28:33Z")
            , C.Status (read "650e8400-e29b-11d4-a716-446655440000")
                       Nothing
                       (Just 12)
                       Nothing
                       (Just 6)
                       (read "2011-11-20 18:28:42Z")
            ]
          service = mkGetStatusService (return records)
          result  = runIdentity service
        in result `shouldBe` [from (head records), from (records !! 1)]

  describe "mkPostStatusService" $ do
    let
      uuid        = read "550e8400-e29b-11d4-a716-446655440000"
      created     = read "2011-11-20 18:28:45Z"
      env         = return @(CatchT Identity) created #: return @(CatchT Identity) uuid #: nil
      request     = StatusRequest (Just 11) (Just 12) (Just 5) (Just 0)
      expectedRow = C.Status uuid (Just 11) (Just 12) (Just 5) (Just 0) created
    it "should map a successfull insertion properly" $ do
      let insertStatusRepository expectedRow = return C.Success
          service = mkPostStatusService insertStatusRepository
          result = runReaderT (service request) env
      case runIdentity (runCatchT result) of
        Right value -> value `shouldBe` from expectedRow
        x -> fail $ "unexpected value" ++ show x

    it "should throw an UUIDCollisionException on PkAlreadyExists summand" $ do
      let insertStatusRepository expectedRow = return C.PkAlreadyExists
          service = mkPostStatusService insertStatusRepository
          result = runReaderT (service request) env
      case runIdentity (runCatchT result) of
        Left (SomeException e) -> show e `shouldBe` "UUIDCollisionException"
        x -> fail $ "unexpected value" ++ show x
