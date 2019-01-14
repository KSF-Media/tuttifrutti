{-# LANGUAGE OverloadedStrings #-}
module Tuttifrutti.Cache.Storage.Postgres where

import           Prelude
import           RIO.List                             (headMaybe)
import qualified Tuttifrutti.Cache.Storage            as Storage
import           Tuttifrutti.Prelude

import qualified Data.Aeson                           as Json
import           Data.Pool                            (Pool)
import qualified Data.Pool                            as Pool
import qualified Data.Range.Range                     as Range
import qualified Data.Text                            as Text
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import qualified Database.PostgreSQL.Simple.Types     as PG
import qualified Tuttifrutti.Persist                  as Persist

newtype TableName = TableName PG.Identifier
  deriving
    ( Eq, Ord, Show, IsString)

newPool
  :: PG.ConnectInfo
  -> IO (Pool PG.Connection)
newPool connectInfo =
  Pool.createPool
    (PG.connect connectInfo)
    PG.close
    1 -- stripes
    (10 * 60) -- seconds, to keep unused connections open
    10 -- maximum number of connections

data Schema k p v = Schema
  { schemaKey      :: (PG.Identifier, PG.Identifier) -- (name, type)
  , schemaPriority :: (PG.Identifier, PG.Identifier) -- (name, type)
  , schemaValue    :: (PG.Identifier, PG.Identifier) -- (name, type)
  }

schemaQuery :: Schema k p v -> PG.Query
schemaQuery Schema{..} = foldMap (fromString . Text.unpack)
  [ "("
  , Text.intercalate "," $
      [ fold [PG.fromIdentifier name, " ", PG.fromIdentifier type_]
      | (name, type_) <- [schemaKey, schemaPriority, schemaValue]
      ]
  , ")"
  ]

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
newHandle (TableName tableName) schema@Schema{..} connectionPool = do
  Pool.withResource connectionPool $ \connection -> do
    r <- PG.execute connection
      ("CREATE TABLE IF NOT EXISTS ? " <> schemaQuery schema)
      [tableName]
    print r
  pure Storage.Handle
    { Storage.alter = \f k -> liftIO $ Pool.withResource connectionPool $ \connection -> do
        existing :: Maybe (p, v) <- fmap (\(_, p, v) -> (p, v)) . headMaybe <$> do
          PG.query connection "SELECT * FROM ? WHERE ? = ?" (tableName, fst schemaKey, k) :: IO [(k, p, v)]
        let (a, new) = f existing
        a <$ case (existing, new) of
          (Nothing, Nothing) -> pure ()
          (Nothing, Just (p, v)) -> do
            void $ PG.execute connection "INSERT INTO ? VALUES (?, ?, ?)"
              (tableName, k, p, v)
            pure ()
          (Just (oldP, oldV), Just (newP, newV))
            | oldP == newP, oldV == newV -> pure ()
            | otherwise -> do
                void $ PG.execute connection "UPDATE ? SET ? = ?, ? = ? WHERE ? = ?"
                  ( tableName
                  , fst schemaPriority, newP
                  , fst schemaValue, newV
                  , fst schemaKey, k
                  )
          (Just _, Nothing) -> do
            void $ PG.execute connection "DELETE FROM ? WHERE ? = ?" (tableName, fst schemaKey, k)
    , Storage.dropRange = \range -> void $ liftIO $ Pool.withResource connectionPool $ \connection ->
        case range of
          Range.SingletonRange k -> PG.execute connection
            "DELETE FROM ? WHERE ? = ?"
            (tableName, fst schemaPriority, k)
          Range.SpanRange k1 k2 -> PG.execute connection
            "DELETE FROM ? WHERE ? >= ?, ? <= ?"
            ( tableName
            , fst schemaPriority, k1
            , fst schemaPriority, k2
            )
          Range.LowerBoundRange k -> PG.execute connection
            "DELETE FROM ? WHERE ? >= ?"
            ( tableName, fst schemaPriority, k)
          Range.UpperBoundRange k -> PG.execute connection
            "DELETE FROM ? WHERE ? <= ?"
            (tableName, fst schemaPriority, k)
          Range.InfiniteRange -> PG.execute connection
            "DELETE FROM ?"
            (PG.Only tableName)

    }
