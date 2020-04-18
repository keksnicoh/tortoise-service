{-# LANGUAGE LambdaCase #-}
module SpecDatabase where

import           Test.Hspec
import           Control.Monad.Reader
import           Core.Database.Env
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
                                                ( Query(Query) )
import           Control.Exception              ( try )
import           Control.Exception.Base         ( SomeException )
import           GHC.Int                        ( Int64 )

withDatabaseMigrated
  :: (HasDbConnection e, HasDbSchema e)
  => e
  -> (Connection -> Expectation)
  -> Expectation
withDatabaseMigrated env spec = do
  let conn = getDbConnection env
  forM_ (Query <$> getDbSchema env) (tryIgnore conn) >> spec conn
 where
  tryIgnore c q = try (execute c q ()) >>= printSchemaError
  printSchemaError :: Either SomeException Int64 -> IO ()
  printSchemaError = \case
    Left err -> do
      putStrLn "[WARN] withDatabaseMigrated error during schema execution"
      print err
    _ -> return ()
