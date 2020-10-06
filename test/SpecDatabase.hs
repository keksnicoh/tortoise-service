{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module SpecDatabase where

import           Control.Exception                (try)
import           Control.Exception.Base           (SomeException)
import           Control.Monad.Reader             (forM_)
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.Types (Query (Query))
import           GHC.Int                          (Int64)
import           OpenEnv                          (Provides, getValue)
import           Test.Hspec                       (Expectation)

import           SpecEnv                          (DbSchema)

withDatabaseMigrated
  :: (Provides Connection e, Provides DbSchema e)
  => e
  -> (e -> Expectation)
  -> Expectation
withDatabaseMigrated env spec = do
  let conn = getValue env
  forM_ (Query <$> getValue @DbSchema env) (tryIgnore conn) >> spec env
 where
  tryIgnore c q = try (execute c q ()) >>= printSchemaError
  printSchemaError :: Either SomeException Int64 -> IO ()
  printSchemaError = \case
    Left err -> do
      putStrLn "[WARN] withDatabaseMigrated error during schema execution"
      print err
    _ -> return ()
