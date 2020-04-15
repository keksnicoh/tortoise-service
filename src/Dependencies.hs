{-# LANGUAGE FlexibleInstances #-}
module Dependencies where

import qualified Data.Time                     as T
import           Data.UUID                      ( UUID )

class HasCurrentTime a where getCurrentTime :: a -> IO T.UTCTime
class HasRandomUUID a  where getRandomUUID  :: a -> IO UUID

instance HasCurrentTime (IO T.UTCTime) where getCurrentTime = id
instance HasRandomUUID (IO UUID)       where getRandomUUID = id