{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Database.Model.StatusSpec where

import           Control.Monad.Reader
import qualified Core.Database.Model.Status    as C
import qualified Data.UUID                     as UUID
import           SpecDatabase
import           Test.Hspec
import           Text.Printf
import           OpenEnv
import           Database.PostgreSQL.Simple
import           SpecEnv

mkSpec :: (Provides Connection e, Provides DbSchema e) => e -> Spec
mkSpec env = do
  describe "insertStatusRepository" $ do
    let record = C.Status UUID.nil
                          (Just 10)
                          (Just 10)
                          (Just 6)
                          Nothing
                          (read "2011-11-19 18:28:23Z")
        insert = runReaderT (C.insertStatusRepository record)
    it "should not allow inserting the same UUID twice"
      $ withDatabaseMigrated env
      $ \dbEnv -> do
          insert dbEnv >>= shouldBe C.Success
          insert dbEnv >>= shouldBe C.PkAlreadyExists
    it "should fully insert the record" $ withDatabaseMigrated env $ \dbEnv ->
      do
        insert dbEnv
        result <- runReaderT (C.mkFetchStatusRepository 2) dbEnv
        result `shouldBe` [record]

  describe "mkFetchStatusRepository" $ do
    let record = C.Status UUID.nil
                          (Just 10)
                          (Just 10)
                          (Just 3)
                          (Just 0)
                          (read "2011-11-19 18:28:33Z")
        records =
          [ record { C.statusId    = read "550e8400-e29b-11d4-a716-446655440000"
                   , C.temperature = Just 1
                   , C.created     = read "2011-11-19 18:28:23Z"
                   }
          , record { C.statusId    = read "650e8400-e29b-11d4-a716-446655440000"
                   , C.temperature = Just 2
                   , C.created     = read "2011-11-13 18:28:23Z"
                   }
          , record { C.statusId    = read "750e8400-e29b-11d4-a716-446655440000"
                   , C.temperature = Just 3
                   , C.created     = read "2011-11-17 18:28:23Z"
                   }
          , record { C.statusId    = read "850e8400-e29b-11d4-a716-446655440000"
                   , C.temperature = Just 4
                   , C.created     = read "2011-11-19 18:28:24Z"
                   }
          ]
    forM_
        [ (5, [records !! 3, head records, records !! 2, records !! 1])
        , (4, [records !! 3, head records, records !! 2, records !! 1])
        , (3, [records !! 3, head records, records !! 2])
        ]
      $ \(n, expected) ->
          it
              (printf
                "should return the defined number of rows ordered by creation date (%d)"
                n
              )
            $ withDatabaseMigrated env
            $ \conn -> do
                let effect = do
                      forM_ records C.insertStatusRepository
                      C.mkFetchStatusRepository n
                runReaderT effect conn >>= flip shouldBe expected

  describe "fetchStatusPeriodRepository" $ do
    let record = C.Status UUID.nil
                          (Just 10)
                          (Just 10)
                          (Just 5)
                          Nothing
                          (read "2011-11-19 18:28:33Z")
        records =
          [ record { C.statusId = read "550e8400-e29b-11d4-a716-446655440000"
                   , C.created  = read "2011-11-19 18:28:23Z"
                   }
          , record { C.statusId = read "650e8400-e29b-11d4-a716-446655440000"
                   , C.created  = read "2011-11-19 18:28:25Z"
                   }
          , record { C.statusId = read "750e8400-e29b-11d4-a716-446655440000"
                   , C.created  = read "2011-11-19 18:28:24Z"
                   }
          , record { C.statusId = read "850e8400-e29b-11d4-a716-446655440000"
                   , C.created  = read "2011-11-19 18:28:27Z"
                   }
          , record { C.statusId = read "950e8400-e29b-11d4-a716-446655440000"
                   , C.created  = read "2011-11-19 18:28:28Z"
                   }
          ]

    forM_
        [ ( "fetch by inclusive boundary - full hull"
          , (read "2011-11-19 18:28:23Z", read "2011-11-19 18:28:28Z")
          , [ records !! 4
            , records !! 3
            , records !! 1
            , records !! 2
            , head records
            ]
          )
        , ( "fetch by inclusive boundary - partial"
          , (read "2011-11-19 18:28:24Z", read "2011-11-19 18:28:27Z")
          , [records !! 3, records !! 1, records !! 2]
          )
        , ( "fetch intersecting boundary"
          , (read "2011-11-19 18:28:26Z", read "2012-11-19 18:28:27Z")
          , [records !! 4, records !! 3]
          )
        ]
      $ \(label, period, expectedRecords) ->
          it label $ withDatabaseMigrated env $ \conn -> do
            let effect = do
                  forM_ records C.insertStatusRepository
                  C.fetchStatusPeriodRepository period
            result <- runReaderT effect conn
            result `shouldBe` expectedRecords
