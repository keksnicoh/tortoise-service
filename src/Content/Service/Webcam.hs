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


type PersistWebcam m = LBS.ByteString -> m ()

mkPersistWebcam
  :: (MonadIO m, MonadReader e m, HasCurrentTime e, HasAssetsPath e)
  => FilePath
  -> UpdateState m
  -> (FilePath -> LBS.ByteString -> IO ())
  -> PersistWebcam m
mkPersistWebcam filePath updateState writeFile payload = do
  assetsPath <- reader getAssetsPath
  now <- reader getCurrentTime >>= liftIO
  updateState (\s -> s { webcamDate = Just now })
  liftIO $ writeFile (assetsPath <> filePath) payload
