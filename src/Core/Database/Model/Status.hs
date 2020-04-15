{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Core.Database.Model.Status
  ( Status(..)
  , InsertStatusRepository
  , FetchStatusRepository
  , FetchStatusPeriodRepository
  , mkFetchStatusRepository
  , insertStatusRepository
  , fetchStatusPeriodRepository
  , InsertStatusRepositoryResult(..)
  )
where

import           Control.Monad.Reader
import           GHC.Generics                   ( Generic )
import           Data.UUID                      ( UUID )
import           Core.Internal                  ( Temperature
                                                , Humidity
                                                )
import           Core.Database.Env              ( HasDbConnection(..) )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Data.Time                      ( UTCTime )

type InsertStatusRepository m = Status -> m InsertStatusRepositoryResult
type FetchStatusRepository m = m [Status]
type FetchStatusPeriodRepository m = (UTCTime, UTCTime) -> m [Status]

data InsertStatusRepositoryResult = Success | PkAlreadyExists
  deriving (Show, Eq)

data Status = Status
  { statusId :: UUID
  , temperature :: Temperature
  , humidity :: Humidity
  , created :: UTCTime
} deriving (Generic, Eq, Show)

instance FromRow Status where
  fromRow = Status <$> field <*> field <*> field <*> field

-- |constructs a repository which returns the last n status rows
-- from storage.
mkFetchStatusRepository
  :: (MonadIO m, MonadReader e m, HasDbConnection e)
  => Int
  -> FetchStatusRepository m
mkFetchStatusRepository n = simpleQuery selectQuery (Only n)
 where
  selectQuery =
    "SELECT status_id, temperature, humidity, created FROM public.status"
      <> " ORDER BY \"created\" DESC"
      <> " LIMIT ?"

-- | consructs a repository which inserts into storage
insertStatusRepository
  :: (MonadIO m, MonadReader e m, HasDbConnection e) => InsertStatusRepository m
insertStatusRepository status = do
  conn <- reader getDbConnection
  liftIO $ do
    count <- countByStatusId conn (statusId status)
    case count of
      [Only 0] -> insert conn status >> return Success
      _        -> return PkAlreadyExists
 where
  insertionQuery =
    "INSERT INTO public.status (\"status_id\", \"temperature\", \"humidity\", \"created\")"
      <> "VALUES (?, ?, ?, ?)"
  selectQuery =
    "SELECT COUNT(\"status_id\") FROM public.status WHERE \"status_id\"=?"
  countByStatusId conn uuid =
    query conn selectQuery (Only uuid) :: IO [Only Int]
  insert conn status = execute
    conn
    insertionQuery
    (statusId status, temperature status, humidity status, created status)

-- |fetches status within a given time frame
fetchStatusPeriodRepository
  :: (MonadIO m, MonadReader e m, HasDbConnection e)
  => FetchStatusPeriodRepository m
fetchStatusPeriodRepository = simpleQuery selectQuery
 where
  selectQuery =
    "SELECT status_id, temperature, humidity, created FROM public.status"
      <> " WHERE created > ? AND created < ?"
      <> " ORDER BY \"created\" DESC"

simpleQuery
  :: (MonadIO m, MonadReader e m, HasDbConnection e, ToRow a, FromRow b)
  => Query
  -> a
  -> m [b]
simpleQuery q p = do
  runner <- query <$> reader getDbConnection
  liftIO $ runner q p
