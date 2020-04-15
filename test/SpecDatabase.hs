module SpecDatabase where

import           Test.Hspec
import           Control.Monad.Reader
import           Core.Database.Env
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
                                                ( Query(Query) )

withDatabaseMigrated
  :: (HasDbConnection e, HasDbSchema e)
  => e
  -> (Connection -> Expectation)
  -> Expectation
withDatabaseMigrated env spec = do
  let conn = getDbConnection env
  forM_ (Query <$> getDbSchema env) $ \q -> execute conn q ()
  spec conn
