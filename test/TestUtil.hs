module TestUtil where

import           Test.Hspec
import           Control.Monad.Reader

mockSingular
  :: (Eq a, Show a, MonadIO m) => (a -> Expectation) -> b -> (a -> m b)
mockSingular expect result arg = liftIO $ expect arg >> return result
