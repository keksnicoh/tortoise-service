{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Content.Service.StatusService
    ( GetStatusService
    , PostStatusService
    , mkGetStatusService
    , mkPostStatusService
    , PostStatusServiceException(..)) where

import           Content.Model.Status        (Status, from)
import           Content.Model.StatusRequest (StatusRequest, toStatus)
import           Control.Monad.Catch         (Exception, MonadThrow (..))
import           Control.Monad.Reader        (MonadReader)
import qualified Core.Database.Model.Status  as C
import qualified Data.Time                   as T
import           Data.UUID                   (UUID)
import           OpenEnv                     (Embedded, embedded)

type GetStatusService m = m [Status]

type PostStatusService m = StatusRequest -> m Status

-- |constructs a service which returns status rows from storage
mkGetStatusService
  :: (Functor m) => C.FetchStatusRepository m -> GetStatusService m
mkGetStatusService = (fmap . fmap) from -- note how nice eta reduction and functoriality

-- worked out here
data PostStatusServiceException = UUIDCollisionException
  deriving (Show, Eq, Exception)

-- |constructs a service which persists a status
mkPostStatusService
  :: (MonadThrow m, MonadReader e m, Embedded T.UTCTime e m, Embedded UUID e m)
  => C.InsertStatusRepository m
  -> PostStatusService m
mkPostStatusService insertStatusRepository request = do
  status <- toStatus request <$> embedded <*> embedded
  insertStatusRepository status
    >>= \case
      C.Success         -> return (from status)
      C.PkAlreadyExists -> throwM UUIDCollisionException
