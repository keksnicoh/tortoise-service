{-# LANGUAGE OverloadedStrings #-}
module Core.OpenWeatherMap.Repository.ForecastSpec where

import           Control.Exception                       (try)
import           Control.Monad.Reader                    (ReaderT (runReaderT))
import           Core.OpenWeatherMap.Env                 (OpenWeatherMapEnv (OpenWeatherMapEnv, managedHttpLbs, weatherUrl))
import           Core.OpenWeatherMap.Model.Forecast      (ForecastResult)
import           Core.OpenWeatherMap.Repository.Forecast (ForecastRepositoryException (..),
                                                          forecastRepository)
import qualified Data.ByteString.Lazy                    as BSL
import           Data.Either                             (isRight)
import           Network.HTTP.Client.Internal            (Request (..),
                                                          Response (..), method)
import           Network.HTTP.Types                      (notFound404, ok200)
import           OpenEnv                                 (nil, ( #: ))
import           Test.Hspec                              (Spec, describe, it,
                                                          shouldBe,
                                                          shouldSatisfy)
import           TestUtil                                (mockSingular)

spec :: Spec
spec = describe "forecastRepository" $ do
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
