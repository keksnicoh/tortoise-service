{-# LANGUAGE OverloadedStrings #-}
module Core.OpenWeatherMap.Repository.ForecastSpec where

import           Core.OpenWeatherMap.Repository.Forecast
import           Core.OpenWeatherMap.Model.Forecast
import           Test.Hspec
import           Network.HTTP.Client.Internal   ( method
                                                , Request(..)
                                                , Response(..)
                                                , newManager
                                                , defaultManagerSettings
                                                )
import           Core.OpenWeatherMap.Env
import           TestUtil
import           Network.HTTP.Types             ( notFound404
                                                , ok200
                                                )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Control.Exception              ( try )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Either                    ( isRight )
import           OpenEnv

spec :: Spec
spec = describe "forecastRepository" $ do
  manager <- runIO $ newManager defaultManagerSettings
  let baseResponse = Response { responseStatus    = undefined
                              , responseVersion   = undefined
                              , responseHeaders   = undefined
                              , responseBody      = ""
                              , responseCookieJar = undefined
                              , responseClose'    = undefined
                              }
      createEnv response =
        OpenWeatherMapEnv
            { weatherUrl     =
              "https://www.a.org/data/2.5/forecast/hourly?myQuery=a"
            , managedHttpLbs = mockSingular
                                 (`shouldSatisfy` \req ->
                                   method req
                                     == "GET"
                                     && secure req
                                     && host req
                                     == "www.a.org"
                                     && path req
                                     == "/data/2.5/forecast/hourly"
                                     && queryString req
                                     == "?myQuery=a"
                                 )
                                 response
            }
          #: nil

  it "throw an UnexpectedHttpStatusException if statusCode != 200" $ do
    let env = createEnv $ baseResponse { responseStatus = notFound404 }
    result <- try (runReaderT forecastRepository env)
    result `shouldBe` Left (UnexpectedHttpStatusException notFound404)

  it "throw an InvalidResponseBodyException if response could not be parsed"
    $ do
        let env = createEnv $ baseResponse
              { responseStatus = ok200
              , responseBody   = "this is not parsable :("
              }
        result <- try (runReaderT forecastRepository env)
        result `shouldBe` Left
          (InvalidResponseBodyException
            "Error in $: string at 'thisisnotparsable:('"
          )

  it "return a forecast by parsing json body 200 response" $ do
    contents <- BSL.readFile
      "test/fixtures/core/open-weather-map/forecast0.json"
    let env = createEnv
          $ baseResponse { responseStatus = ok200, responseBody = contents }
    result <-
      try (runReaderT forecastRepository env) :: IO
        (Either ForecastRepositoryException ForecastResult)
    result `shouldSatisfy` isRight
