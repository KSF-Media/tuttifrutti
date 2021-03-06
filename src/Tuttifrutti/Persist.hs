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
import           Data.Text.Encoding                  (decodeUtf8)
import           Database.Persist
import           Database.Persist.Postgresql         (Migration, Sql, SqlBackend,
                                                      createPostgresqlPool, parseMigration,
                                                      runMigrationSilent, runSqlConn, runSqlPool)
import qualified Database.Persist.Postgresql         as Persist
import           Database.Persist.Sql.Types.Internal (LogFunc, connLogFunc)
import           Database.PostgreSQL.Simple          (SqlError (..))
import qualified System.Log.FastLogger               as FastLogger

import qualified Tuttifrutti.Log                     as Log
import qualified Tuttifrutti.Log.Handle              as Log
import           Tuttifrutti.Pool                    (Pool)
import qualified Tuttifrutti.Pool                    as Pool
import qualified Tuttifrutti.Postgres                as Postgres

type MonadPersist env m =
  (MonadReader env m, Has Handle env, MonadUnliftIO m, MonadIO m, Log.MonadLog env m)

newtype Handle = Handle { handlePool :: Pool SqlBackend }

newSqlBackendPool
  :: Log.Handle
  -> Postgres.Config
  -> IO (Pool Persist.SqlBackend)
newSqlBackendPool logHandle config@Postgres.Config{..} =
  Pool.createPool
    (connect logHandle config)
    disconnect
    configPoolConfig

connect :: Log.Handle -> Postgres.Config -> IO SqlBackend
connect logHandle config = do
  connection <- Postgres.connect logHandle config
  Persist.openSimpleConn (logFunc logHandle) connection

disconnect :: SqlBackend -> IO ()
disconnect = Persist.close'

-- | Create a Handle for the Postgres Pool.
postgresHandle :: Log.Handle -> Pool SqlBackend -> Migration -> IO Handle
postgresHandle logHandle connectionPool migration = do
  let dbHandle = Handle connectionPool
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

