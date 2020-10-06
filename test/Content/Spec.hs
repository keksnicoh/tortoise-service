module Content.Spec where

import qualified Content.Model.MonitorSpec
import qualified Content.Model.StatusRequestSpec
import qualified Content.Model.StatusSpec
import qualified Content.Model.SwitchSpec
import qualified Content.Model.TimeSeriesSpec
import qualified Content.Service.StatusServiceSpec
import qualified Content.Service.TimeSeriesServiceSpec
import qualified Content.Service.WebcamServiceSpec
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  describe "Content.Model.MonitorSpec" Content.Model.MonitorSpec.spec
  describe "Content.Model.StatusRequestSpec" Content.Model.StatusRequestSpec.spec
  describe "Content.Model.StatusSpec" Content.Model.StatusSpec.spec
  describe "Content.Model.SwitchSpec" Content.Model.SwitchSpec.spec
  describe "Content.Model.TimeSeriesSpec" Content.Model.TimeSeriesSpec.spec

  describe "Content.Service.StatusServiceSpec" Content.Service.StatusServiceSpec.spec
  describe "Content.Service.TimeSeriesServiceSpec" Content.Service.TimeSeriesServiceSpec.spec
  describe "Content.Service.WebcamServiceSpec" Content.Service.WebcamServiceSpec.spec
