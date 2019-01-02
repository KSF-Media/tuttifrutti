{-# LANGUAGE OverloadedStrings #-}
module Tuttifrutti.Persist
  ( module Database.Persist
  , module Database.Persist.Postgresql
  , module Database.PostgreSQL.Simple
  , module Tuttifrutti.Persist
  ) where

import           Tuttifrutti.Prelude

import qualified Control.Monad.Logger                as MonadLogger
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

import qualified Tuttifrutti.Log                     as Log
import qualified Tuttifrutti.Log.Handle              as Log

type MonadPersist env m =
  (MonadReader env m, Has Handle env, MonadUnliftIO m, MonadIO m, Log.MonadLog env m)

newtype Handle = Handle { unHandle :: Pool SqlBackend }

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

createConnectionPool :: Log.Handle -> PG.ConnectInfo -> IO (Pool SqlBackend)
createConnectionPool logHandle connectInfo = do
  connection <- PG.connect connectInfo
  idleTimeout <- hush <$> do Envy.runEnv $ Envy.env "POSTGRES_POOL_IDLE_TIMEOUT"
  stripesAmount <- hush <$> do Envy.runEnv $ Envy.env "POSTGRES_POOL_STRIPES"
  connectionAmount <- hush <$> do Envy.runEnv $ Envy.env "POSTGRES_POOL_CONNECTIONS"
  Pool.createPool
    (Persist.openSimpleConn (logFunc logHandle) connection)
    Persist.close'
    (fromMaybe 1 stripesAmount)
    (maybe 600 fromInteger idleTimeout)
    (fromMaybe 10 connectionAmount)

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
closeHandle = Pool.destroyAllResources . unHandle

-- | Our type synonym for environment that allows accessing 'SqlBackend'.
type QueryT m = ReaderT SqlBackend m

-- | Run a query in a transaction.
transact :: (MonadPersist env m) => QueryT m a -> m a
transact m = do
  pool <- asks (unHandle . Has.getter)
  logHandle <- asks Has.getter
  runSqlPool
    -- we update the logging function to use current log handle
    (setLogFunc logHandle m)
    pool
  where
    setLogFunc logHandle =
      local $ \conn -> conn
        { connLogFunc = logFunc logHandle }

