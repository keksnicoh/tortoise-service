module Content.Service.Webcam where

import           Core.State.Repository.State
import           Core.State.Model.State
import           Dependencies
import           Control.Monad.Reader           ( liftIO
                                                , reader
                                                , MonadIO
                                                , MonadReader
                                                )
import qualified Data.ByteString.Lazy          as LBS


type PersistWebcam m = FilePath -> LBS.ByteString -> m ()

mkPersistWebcam
  :: (MonadIO m, MonadReader e m, HasCurrentTime e)
  => UpdateState m
  -> (FilePath -> LBS.ByteString -> IO ())
  -> PersistWebcam m
mkPersistWebcam updateState writeFile filepath payload = do
  now <- reader getCurrentTime >>= liftIO
  updateState (\s -> s { webcamDate = Just now })
  liftIO $ writeFile filepath payload
