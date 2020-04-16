{-# LANGUAGE LambdaCase #-}
module Content.Service.Status
  ( GetStatusService
  , PostStatusService
  , mkGetStatusService
  , mkPostStatusService
  )
where

import           Control.Monad.Reader
import           Content.Model.Status           ( Status
                                                , from
                                                )
import           Content.Model.StatusRequest    ( StatusRequest
                                                , toStatus
                                                )
import qualified Core.Database.Model.Status    as C
import           Dependencies

type GetStatusService m = m [Status]
type PostStatusService m = StatusRequest -> m Status

-- |constructs a service which returns status rows from storage
mkGetStatusService
  :: (Monad m) => C.FetchStatusRepository m -> GetStatusService m
mkGetStatusService = (fmap . fmap) from -- note how nice eta reduction and functoriality
                                        -- worked out here, just very cool!

-- |constructs a service which persists a status
mkPostStatusService
  :: (MonadIO m, MonadReader e m, HasCurrentTime e, HasRandomUUID e)
  => C.InsertStatusRepository m
  -> PostStatusService m
mkPostStatusService insertStatusRepository request = do
  currentTime <- reader getCurrentTime
  randomUUID  <- reader getRandomUUID
  status      <- liftIO $ toStatus request <$> randomUUID <*> currentTime
  insertStatusRepository status >>= \case
    C.Success -> return (from status)
    _         -> error "unexpected duplication error" -- todo Throw something ...
