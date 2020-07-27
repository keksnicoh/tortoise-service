{-# LANGUAGE OverloadedStrings #-}

module Content.Handler.WebcamHandler
  ( mkWebcamHandler
  )
where

import           Control.Monad.Reader
import           Servant
import           Content.Service.WebcamService
import           Servant.Multipart

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
