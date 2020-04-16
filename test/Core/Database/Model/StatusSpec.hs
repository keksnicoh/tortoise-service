{-# LANGUAGE OverloadedStrings #-}

module Core.Database.Model.StatusSpec where

import Control.Monad.Reader
import Core.Database.Env
import qualified Core.Database.Model.Status as C
import qualified Data.UUID as UUID
import SpecDatabase
import Test.Hspec
import Text.Printf

mkSpec :: (HasDbConnection e, HasDbSchema e) => e -> Spec
mkSpec env = do

  describe "insertStatusRepository" $ do
    let record = C.Status UUID.nil 10 10 (read "2011-11-19 18:28:23")
        insert = runReaderT (C.insertStatusRepository record)
    it "should not allow inserting the same UUID twice" $ withDatabaseMigrated env $ \conn -> do
      insert conn >>= shouldBe C.Success
      insert conn >>= shouldBe C.PkAlreadyExists
    it "should fully insert the record" $ withDatabaseMigrated env $ \conn -> do
      insert conn
      result <- runReaderT (C.mkFetchStatusRepository 2) conn
      result `shouldBe` [record]

  describe "mkFetchStatusRepository" $ do
    let record = C.Status UUID.nil 10 10 (read "2011-11-19 18:28:33")
        records =
          [ record
              { C.statusId = read "550e8400-e29b-11d4-a716-446655440000"
              , C.temperature = 1
              , C.created = read "2011-11-19 18:28:23"
              }
          , record
              { C.statusId = read "650e8400-e29b-11d4-a716-446655440000"
              , C.temperature = 2
              , C.created = read "2011-11-13 18:28:23"
              }
          , record
              { C.statusId = read "750e8400-e29b-11d4-a716-446655440000"
              , C.temperature = 3
              , C.created = read "2011-11-17 18:28:23"
              }
          , record
              { C.statusId = read "850e8400-e29b-11d4-a716-446655440000"
              , C.temperature = 4
              , C.created = read "2011-11-19 18:28:24"
              }
          ]
    forM_
      [ (5, [records !! 3, head records, records !! 2, records !! 1])
      , (4, [records !! 3, head records, records !! 2, records !! 1])
      , (3, [records !! 3, head records, records !! 2])
      ] $ \(n, expected) -> 
        it (printf "should return the defined number of rows ordered by creation date (%d)" n)
          $ withDatabaseMigrated env $ \conn -> do
            result <- runReaderT (forM_ records C.insertStatusRepository >> C.mkFetchStatusRepository n) conn
            result `shouldBe` expected

  describe "fetchStatusPeriodRepository" $ do
    let record = C.Status UUID.nil 10 10 (read "2011-11-19 18:28:33")
        records =
          [ record
              { C.statusId = read "550e8400-e29b-11d4-a716-446655440000"
              , C.created = read "2011-11-19 18:28:23"
              }
          , record
              { C.statusId = read "650e8400-e29b-11d4-a716-446655440000"
              , C.created = read "2011-11-19 18:28:25"
              }
          , record
              { C.statusId = read "750e8400-e29b-11d4-a716-446655440000"
              , C.created = read "2011-11-19 18:28:24"
              }
          , record
              { C.statusId = read "850e8400-e29b-11d4-a716-446655440000"
              , C.created = read "2011-11-19 18:28:27"
              }
          , record
              { C.statusId = read "950e8400-e29b-11d4-a716-446655440000"
              , C.created = read "2011-11-19 18:28:28"
              }
          ]

    forM_
      [ ( "fetch by inclusive boundary - full hull"
        , (read "2011-11-19 18:28:23", read "2011-11-19 18:28:28")
        , [records !! 4, records !! 3, records !! 1, records !! 2, head records]
        )
      , ( "fetch by inclusive boundary - partial"
        , (read "2011-11-19 18:28:24", read "2011-11-19 18:28:27")
        , [records !! 3, records !! 1, records !! 2]
        )
      , ( "fetch intersecting boundary"
        , (read "2011-11-19 18:28:26", read "2012-11-19 18:28:27")
        , [records !! 4, records !! 3]
        )
      ] $ \(label, period, expectedRecords) -> 
        it label $ withDatabaseMigrated env $ \conn -> do
          result <- runReaderT (forM_ records C.insertStatusRepository >> C.fetchStatusPeriodRepository period) conn
          result `shouldBe` expectedRecords
