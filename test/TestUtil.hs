module TestUtil where

import           Control.Monad.Reader (MonadIO (..))
import           Test.Hspec           (Expectation, shouldBe)

mockSingular
  :: (MonadIO m) => (a -> Expectation) -> b -> (a -> m b)
mockSingular expect result arg = liftIO $ expect arg >> return result

mockTwo
  :: (MonadIO m) => (a -> Expectation) -> (b -> Expectation) -> c -> (a -> b -> m c)
mockTwo e1 e2 result arg1 arg2 = liftIO $ e1 arg1 >> e2 arg2 >> return result

(>>?=) :: (Show a, Eq a) => IO a -> a -> IO ()
a >>?= b = a >>= flip shouldBe b
