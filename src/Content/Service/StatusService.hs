{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Content.Service.StatusService
  ( GetStatusService
  , PostStatusService
  , mkGetStatusService
  , mkPostStatusService
  , PostStatusServiceException(..)
  )
where

import           Control.Monad.Reader           ( MonadReader )
import           Content.Model.Status           ( Status
                                                , from
                                                )
import           Content.Model.StatusRequest    ( StatusRequest
                                                , toStatus
                                                )
import           OpenEnv                        ( embedded
                                                , Embedded
                                                )
import qualified Data.Time                     as T
import           Data.UUID                      ( UUID )
import           Control.Monad.Catch            ( Exception
                                                , MonadThrow(..)
                                                )
import qualified Core.Database.Model.Status    as C

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
  insertStatusRepository status >>= \case
    C.Success         -> return (from status)
    C.PkAlreadyExists -> throwM UUIDCollisionException
