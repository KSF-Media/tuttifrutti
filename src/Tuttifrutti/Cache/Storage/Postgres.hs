{-# LANGUAGE OverloadedStrings #-}
-- | A PostgreSQL backend for 'Tuttifrutti.Cache.Storage' interface:
--
--   >>> import Data.Time
--   >>> import Tuttifrutti.Prelude
--   >>> import qualified Tuttifrutti.Log.Handle as Log
--   >>> import qualified Tuttifrutti.Cache.Storage as Storage
--   >>> import qualified Tuttifrutti.Cache.Storage.Postgres as Storage.Postgres
--   >>>
--   >>> logHandle <- Log.newStdoutHandle "tuttifrutti" Log.devMessage
---  >>> let connect = Postgres.connect logHandle PG.defaultConnectInfo Postgres.defaultRetryPolicy
--   >>> pool <- Pool.createPool connect PG.close Pool.defaultConfig
--   >>> h :: Storage.Handle Text UTCTime Json.Value IO <- newHandle "mytable" defaultSchema pool
--   >>>
--   >>> Storage.insert h ("foo", UTCTime (fromGregorian 2018 1 1) 0, object [ "foo" .= "foo" ])
--   >>> Storage.insert h ("bar", UTCTime (fromGregorian 2018 2 2) 0, object [ "bar" .= "bar" ])
--   >>> Storage.insert h ("baz", UTCTime (fromGregorian 2018 3 3) 0, object [ "baz" .= "baz" ])
--   >>>
--   >>> Storage.lookupValid h "bar" (const Just)
---  Just (Object (fromList [("bar",String "bar")]))
--   >>> Storage.lookupValid h "qux" (const Just)
---  Nothing
module Tuttifrutti.Cache.Storage.Postgres where

import           Prelude
import           RIO.List                             (headMaybe)

import qualified Data.Aeson                           as Json
import qualified Data.Range                           as Range
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import qualified Database.PostgreSQL.Simple.Types     as PG

import qualified Tuttifrutti.Cache.Storage            as Storage
import           Tuttifrutti.Pool                     (Pool)
import qualified Tuttifrutti.Pool                     as Pool
import           Tuttifrutti.Prelude

newtype TableName = TableName PG.Identifier
  deriving
    (Eq, Ord, Show, IsString, PG.ToField)

data Schema k p v = Schema
  { schemaKey      :: (PG.Identifier, PG.Identifier) -- (name, type)
  , schemaPriority :: (PG.Identifier, PG.Identifier) -- (name, type)
  , schemaValue    :: (PG.Identifier, PG.Identifier) -- (name, type)
  }

defaultSchema :: Schema Text UTCTime Json.Value
defaultSchema = Schema
  { schemaKey      = ("key", "text")
  , schemaPriority = ("timestamp", "timestamptz")
  , schemaValue    = ("value", "json")
  }

newHandle
  :: forall k p v m
   . MonadIO m
  => (PG.FromField k, PG.FromField p, PG.FromField v)
  => (PG.ToField k, PG.ToField p, PG.ToField v)
  => (Eq p, Eq v)
  => TableName
  -> Schema k p v
  -> Pool PG.Connection
  -> IO (Storage.Handle k p v m)
newHandle tableName schema connectionPool = do
  void $ Pool.withResource connectionPool $ createTable tableName schema
  pure Storage.Handle
    { Storage.alter = \f k ->
        liftIO
          $ Pool.withResource connectionPool
          $ \connection -> alter tableName schema connection f k
    , Storage.dropRange = \range ->
        void
          $ liftIO
          $ Pool.withResource connectionPool
          $ \connection -> dropRange tableName schema connection range

    }

createTable :: forall k p v. TableName -> Schema k p v -> PG.Connection -> IO ()
createTable tableName Schema{..} connection = do
  void $ PG.execute connection
    ("CREATE TABLE IF NOT EXISTS ? (? ?, ? ?, ? ?)")
    ( tableName
    , fst schemaKey,      snd schemaKey
    , fst schemaPriority, snd schemaPriority
    , fst schemaValue,    snd schemaValue
    )
  void $ PG.execute connection
    ( "DO $$ BEGIN IF NOT EXISTS \
      \(SELECT constraint_name FROM information_schema.table_constraints \
      \WHERE table_name = ? AND constraint_type = 'PRIMARY KEY') THEN \
      \ALTER TABLE ? ADD PRIMARY KEY (?); \
      \END IF; END $$"
    )
    ( PG.fromIdentifier $ (\(TableName t) -> t) tableName
    , tableName
    , fst schemaKey
    )

alter
  :: forall k p v a
   . ( PG.FromField k, PG.FromField p, PG.FromField v
     , PG.ToField k, PG.ToField p, PG.ToField v
     , Eq p, Eq v
     )
  => TableName
  -> Schema k p v
  -> PG.Connection
  -> (Maybe (p, v) -> (a, Maybe (p, v)))
  -> k
  -> IO a
alter tableName Schema{..} connection f k = do
  existing :: Maybe (p, v) <- fmap (\(_, p, v) -> (p, v)) . headMaybe <$> do
    PG.query @(TableName, PG.Identifier, k) @(k, p, v) connection
      "SELECT * FROM ? WHERE ? = ?"
      (tableName, fst schemaKey, k)
  let (a, new) = f existing
  a <$ case (existing, new) of
    (Nothing, Nothing) -> pure ()
    (Nothing, Just (p, v)) -> do
      void $ PG.execute connection
        "INSERT INTO ? VALUES (?, ?, ?) \
        \ON CONFLICT (?) DO UPDATE SET ? = EXCLUDED.?, ? = EXCLUDED.?"
        ( tableName, k, p, v
        , fst schemaKey, fst schemaValue, fst schemaValue, fst schemaPriority, fst schemaPriority
        )
      pure ()
    (Just (oldP, oldV), Just (newP, newV))
      | oldP == newP, oldV == newV -> pure ()
      | otherwise -> do
          void $ PG.execute connection
            "UPDATE ? SET ? = ?, ? = ? WHERE ? = ?"
            ( tableName
            , fst schemaPriority, newP
            , fst schemaValue, newV
            , fst schemaKey, k
            )
    (Just _, Nothing) -> do
      void $ PG.execute connection "DELETE FROM ? WHERE ? = ?" (tableName, fst schemaKey, k)

dropRange
  :: forall k p v
   . PG.ToField p
  => TableName
  -> Schema k p v
  -> PG.Connection
  -> Range.Range p
  -> IO ()
dropRange tableName Schema{..} connection range =
  void $ case range of
    Range.SingletonRange k -> PG.execute connection
      "DELETE FROM ? WHERE ? = ?"
      (tableName, fst schemaPriority, k)
    Range.SpanRange (Range.boundValue -> k1) (Range.boundValue -> k2) -> PG.execute connection
      "DELETE FROM ? WHERE ? >= ?, ? <= ?"
      ( tableName
      , fst schemaPriority, k1
      , fst schemaPriority, k2
      )
    Range.LowerBoundRange (Range.boundValue -> k) -> PG.execute connection
      "DELETE FROM ? WHERE ? >= ?"
      ( tableName, fst schemaPriority, k)
    Range.UpperBoundRange (Range.boundValue -> k) -> PG.execute connection
      "DELETE FROM ? WHERE ? <= ?"
      (tableName, fst schemaPriority, k)
    Range.InfiniteRange -> PG.execute connection
      "DELETE FROM ?"
      (PG.Only tableName)
