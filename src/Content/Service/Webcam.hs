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

-- | create an instance of (PersistWebcam m) which modifies the webcamDate
-- state member to the currentTime and uses a `writeFile` handle to persist
-- the given bytestring.
-- 
-- todo: open the bytestring with something like juicepixel in order to ensure
--       that it contains a valid jpeg.
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
