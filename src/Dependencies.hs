{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Dependencies where

import qualified Data.Time                     as T
import           Data.UUID                      ( UUID )

class HasAssetsPath a where 
  getAssetsPath  :: a -> FilePath

class HasCurrentTime r m | r -> m where
  getCurrentTime :: r -> m T.UTCTime

class HasRandomUUID r m | r -> m where
  getRandomUUID  :: r -> m UUID

class HasLogger r m | r -> m where
  getLogger :: r -> String -> m ()

--instance HasCurrentTime (m T.UTCTime) m where
--  getCurrentTime = id

instance HasRandomUUID (m UUID) m where
  getRandomUUID = id

instance HasAssetsPath FilePath where
  getAssetsPath = id
  
instance HasLogger (String -> m ()) m where
  getLogger = id
