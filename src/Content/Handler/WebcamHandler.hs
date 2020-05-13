{-# LANGUAGE OverloadedStrings #-}

module Content.Handler.WebcamHandler
  ( mkWebcamHandler
  )
where

import           Control.Monad.Reader
import           Servant
import           Env
import           Content.Service.WebcamService
import           Servant.Multipart

mkWebcamHandler
  :: PersistWebcam (ReaderT (Env Handler) Handler)
  -> MultipartData Mem
  -> ReaderT (Env Handler) Handler ()
mkWebcamHandler persistWebcam multipartData = do
  case files multipartData of
    [FileData "webcam" "webcam.jpg" "image/jpg" payload] ->
      persistWebcam payload
    _ -> throwError err400
  return ()
