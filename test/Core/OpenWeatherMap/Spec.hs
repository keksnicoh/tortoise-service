module Core.OpenWeatherMap.Spec where

import           Test.Hspec

import qualified Core.OpenWeatherMap.Model.ForecastSpec

import qualified Core.OpenWeatherMap.Repository.ForecastSpec

spec :: Spec
spec = do
  describe "Core.OpenWeatherMap.Model.ForecastSpec"
           Core.OpenWeatherMap.Model.ForecastSpec.spec
  describe "Core.OpenWeatherMap.Repository.ForecastSpec"
           Core.OpenWeatherMap.Repository.ForecastSpec.spec
