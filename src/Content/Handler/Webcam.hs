{-# LANGUAGE OverloadedStrings #-}
module Content.Handler.Webcam where

import           Control.Monad.Reader
import           Servant
import           Env
import           Content.Service.Webcam
import           Servant.Multipart

mkWebcamHandler
  :: PersistWebcam (ReaderT Env Handler)
  -> MultipartData Mem
  -> ReaderT Env Handler ()
mkWebcamHandler persistWebcam multipartData = do
  case files multipartData of
    [FileData "webcam" "webcam.jpg" "image/jpg" payload] ->
      persistWebcam "webcam.jpg" payload
    _ -> throwError err400
  return ()
