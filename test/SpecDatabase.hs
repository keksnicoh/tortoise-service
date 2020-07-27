{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module SpecDatabase where

import           Test.Hspec
import           Control.Monad.Reader
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
                                                ( Query(Query) )
import           Control.Exception              ( try )
import           Control.Exception.Base         ( SomeException )
import           GHC.Int                        ( Int64 )
import OpenEnv

import SpecEnv

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
