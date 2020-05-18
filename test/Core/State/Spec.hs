module Core.State.Spec where

import           Test.Hspec

import qualified Core.State.Model.StateSpec

import qualified Core.State.Repository.StateSpec

import qualified Core.State.EnvSpec

spec :: Spec
spec = do
  describe "Core.State.Model.StateSpec" Core.State.Repository.StateSpec.spec
  describe "Core.State.Repository.StateSpec" Core.State.Model.StateSpec.spec
  describe "Core.State.EnvSpec" Core.State.EnvSpec.spec
