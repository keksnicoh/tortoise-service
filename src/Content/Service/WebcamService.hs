module Content.Service.WebcamService
  ( PersistWebcam
  , mkPersistWebcam
  , RequestWebcamService
  , mkRequestWebcamService
  )
where

import qualified Core.State.Model.State        as CState
import qualified Dependencies                  as D
import           Control.Monad.Reader           ( join
                                                , liftIO
                                                , reader
                                                , MonadIO
                                                , MonadReader
                                                )
import qualified Data.ByteString.Lazy          as LBS
import qualified Core.State.Repository.State   as CS

type PersistWebcam m = LBS.ByteString -> m ()
type RequestWebcamService m = m ()

-- |create an instance of (PersistWebcam m) which modifies the `webcamDate`
-- state member to the currentTime and uses a `writeFile` handle to persist
-- the given bytestring.
-- 
-- todo: open the bytestring with something like juicepixel in order to ensure
--       that it contains a valid jpeg.
mkPersistWebcam
  :: (MonadIO m, MonadReader e m, D.HasCurrentTime e m, D.HasAssetsPath e)
  => FilePath
  -> CS.UpdateState m
  -> (FilePath -> LBS.ByteString -> IO ())
  -> PersistWebcam m
mkPersistWebcam filePath updateState writeFile payload = do
  assetsPath <- reader D.getAssetsPath
  now        <- join (reader D.getCurrentTime)
  updateState (\s -> s { CState.webcamDate = Just now })
  liftIO $ writeFile (assetsPath <> filePath) payload

-- |create an instance of (RequestWebcamService m) which modifies the `webcamRequest`
-- state member to trigger webcam action (`Stream.Service.Action`)
mkRequestWebcamService
  :: (MonadReader e m, D.HasCurrentTime e m)
  => CS.UpdateState m
  -> RequestWebcamService m
mkRequestWebcamService updateState = do
  now <- join (reader D.getCurrentTime)
  updateState (\s -> s { CState.webcamRequest = Just now })
