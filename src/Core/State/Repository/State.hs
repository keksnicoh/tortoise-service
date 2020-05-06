module Core.State.Repository.State
  ( UpdateState
  , updateState
  , GetState
  , currentState
  )
where

import           Core.State.Model.State         ( State )
import           Core.State.Env                 ( HasState(..) )
import           Control.Monad.Reader           ( liftIO
                                                , MonadIO
                                                , MonadReader
                                                , reader
                                                )
import           Data.IORef                     ( readIORef
                                                , atomicModifyIORef'
                                                )

type UpdateState m = (State -> State) -> m ()
type GetState m = m State

updateState :: (MonadIO m, MonadReader e m, HasState e) => UpdateState m
updateState modify = do
  ioRef <- reader getState
  liftIO $ atomicModifyIORef' ioRef $ \state -> (modify state, ())

currentState :: (MonadIO m, MonadReader e m, HasState e) => GetState m
currentState = reader getState >>= liftIO . readIORef
