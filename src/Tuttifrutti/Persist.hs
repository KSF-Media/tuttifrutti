{-# LANGUAGE OverloadedStrings #-}
module Tuttifrutti.Persist
  ( module Database.Persist
  , module Database.Persist.Postgresql
  , module Database.PostgreSQL.Simple
  , module Tuttifrutti.Persist
  ) where

import           Tuttifrutti.Prelude

import           Control.Monad.Catch                 (Handler (..))
import qualified Control.Monad.Logger                as MonadLogger
import           Control.Retry                       (RetryPolicy)
import qualified Control.Retry                       as Retry
import qualified Data.Has                            as Has
import           Data.Pool                           (Pool)
import qualified Data.Pool                           as Pool
import           Data.Text.Encoding                  (decodeUtf8)
import           Database.Persist
import           Database.Persist.Postgresql         (Migration, Sql, SqlBackend,
                                                      createPostgresqlPool, parseMigration,
                                                      runMigrationSilent, runSqlConn, runSqlPool)
import qualified Database.Persist.Postgresql         as Persist
import           Database.Persist.Sql.Types.Internal (LogFunc, connLogFunc)
import           Database.PostgreSQL.Simple          (SqlError (..))
import qualified Database.PostgreSQL.Simple          as PG
import qualified System.Envy                         as Envy
import qualified System.Log.FastLogger               as FastLogger

import qualified GHC.IO.Exception                    as Exception
import qualified Tuttifrutti.Log                     as Log
import qualified Tuttifrutti.Log.Handle              as Log

type MonadPersist env m =
  (MonadReader env m, Has Handle env, MonadUnliftIO m, MonadIO m, Log.MonadLog env m)

newtype Handle = Handle { handlePool :: Pool SqlBackend }

-- | Read from environment vars all the parameters to make a ConnectInfo.
--   The variable name to pass the password in needs to be provided.
getConnectInfo :: String -> IO (Either String PG.ConnectInfo)
getConnectInfo passwordEnvVar = liftIO $ Envy.runEnv $ do
  connectHost     <- Envy.env "POSTGRES_HOST"    <|> pure "localhost"
  connectPort     <- Envy.env "POSTGRES_PORT"    <|> pure 5432
  connectUser     <- Envy.env "POSTGRES_USER"    <|> pure "postgres"
  connectDatabase <- Envy.env "POSTGRES_DB_NAME" <|> pure ""
  connectPassword <- Envy.env passwordEnvVar
  pure PG.ConnectInfo{..}

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  -- exponential backoff starting at 0.1 second
  Retry.exponentialBackoff (round @Double 0.1e6)
    -- with overall timeout of 1 minute
    & Retry.limitRetriesByCumulativeDelay (round @Double 60e6)

createConnectionPool :: Log.Handle -> PG.ConnectInfo -> IO (Pool SqlBackend)
createConnectionPool logHandle connectInfo = do
  idleTimeout <- hush <$> do Envy.runEnv $ Envy.env "POSTGRES_POOL_IDLE_TIMEOUT"
  stripesAmount <- hush <$> do Envy.runEnv $ Envy.env "POSTGRES_POOL_STRIPES"
  connectionAmount <- hush <$> do Envy.runEnv $ Envy.env "POSTGRES_POOL_CONNECTIONS"
  Pool.createPool connect disconnect
    (fromMaybe 1 stripesAmount)
    (maybe 600 fromInteger idleTimeout)
    (fromMaybe 10 connectionAmount)
  where
    connect =
      Retry.recovering defaultRetryPolicy handlers $ \Retry.RetryStatus{..} -> do
        when (rsIterNumber > 0) $ do
          with logHandle $ do
            Log.logInfo "Retrying to connect to database (${iter_number})"
              [ "iter_number" .= rsIterNumber
              , "cumulative_delay" .= rsCumulativeDelay
              , "previous_delay" .= rsPreviousDelay
              ]
        Persist.openSimpleConn (logFunc logHandle) =<< PG.connect connectInfo
    handlers =
      [ \_retryStatus -> Handler $ \Exception.IOError{..} -> do
          with logHandle $ do
            Log.logError "An error occurred in ${location} when connecting to postgres."
              [ "type" .= show ioe_type
              , "location" .= ioe_location
              , "description" .= ioe_description
              , "errno" .= fmap show ioe_errno
              , "filename" .= ioe_filename
              , "handle" .= fmap show ioe_handle
              ]
          -- retry only if the error was raised by libpq
          pure $ ioe_location == "libpq"
      ]
    disconnect = Persist.close'

-- | Create a Handle for the Postgres Pool
postgresHandle :: Log.Handle -> PG.ConnectInfo -> Migration -> IO Handle
postgresHandle logHandle connectInfo migration = do
  dbHandle <- Handle <$> createConnectionPool logHandle connectInfo
  with (logHandle, dbHandle) $ do
    migrationInfo <- transact $ parseMigration migration
    case migrationInfo of
      Left (errors :: [Text]) -> do
        Log.logError "Faulty migration. Can't be performed." [ "errors" .= errors ]
      Right (cautiousMigration :: [(Bool, Sql)])
        -- any unsafe migrations?
        | any fst cautiousMigration -> do
            Log.logWarning "UNSAFE SQL migration."
              [ "migrations" .= cautiousMigrationJson cautiousMigration ]
        | otherwise -> do
            Log.logInfo "Performing SQL migration."
              [ "migrations" .= cautiousMigrationJson cautiousMigration ]
    void $ transact $ runMigrationSilent migration
  pure dbHandle
  where
    cautiousMigrationJson =
      map $ \(unsafe, sql) -> object
        [ "unsafe" .= unsafe
        , "query"  .= sql
        ]

logFunc :: Log.Handle -> LogFunc
logFunc logHandle _loc _source level msg = do
  with logHandle $ Log.localDomain "persistent" $ do
    Log.logMessageNow
      (case level of
         MonadLogger.LevelError   -> Log.LogError
         MonadLogger.LevelDebug   -> Log.LogTrace
         MonadLogger.LevelInfo    -> Log.LogInfo
         MonadLogger.LevelWarn    -> Log.LogWarning
         MonadLogger.LevelOther _ -> Log.LogInfo)
      "Running persistent SQL query"
      payload
  where
    payload = [ "query" .= decodeUtf8 (FastLogger.fromLogStr msg) ]

closeHandle :: Handle -> IO ()
closeHandle = Pool.destroyAllResources . handlePool

-- | Our type synonym for environment that allows accessing 'SqlBackend'.
type QueryT m = ReaderT SqlBackend m

-- | Run a query in a transaction.
transact :: (MonadPersist env m) => QueryT m a -> m a
transact m = do
  Handle{..} <- asks Has.getter
  logHandle <- asks Has.getter
  runSqlPool
    -- we update the logging function to use current log handle
    (setLogFunc logHandle m)
    handlePool
  where
    setLogFunc logHandle =
      local $ \conn -> conn
        { connLogFunc = logFunc logHandle }

