{-# LANGUAGE OverloadedStrings #-}

module Content.Handler.WebcamHandler
  ( mkWebcamHandler
  )
where

import           Content.Service.WebcamService (PersistWebcam)
import           Control.Monad.Reader          (ReaderT)
import           Servant                       (Handler, err400, throwError)
import           Servant.Multipart             (FileData (FileData), Mem,
                                                MultipartData (files))

mkWebcamHandler
  :: PersistWebcam (ReaderT e Handler)
  -> MultipartData Mem
  -> ReaderT e Handler ()
mkWebcamHandler persistWebcam multipartData = do
  case files multipartData of
    [FileData "webcam" "webcam.jpg" "image/jpg" payload] ->
      persistWebcam payload
    _ -> throwError err400
  return ()
