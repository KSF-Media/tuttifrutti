module Tuttifrutti.Postgres
  ( module Tuttifrutti.Postgres
  , module Database.PostgreSQL.Simple
  ) where

import           Control.Monad.Catch                  (Handler (..))
import           Control.Retry                        (RetryPolicy)
import qualified Control.Retry                        as Retry
import qualified Data.Aeson                           as Json
import qualified Data.Has                             as Has
import qualified Data.Text.Encoding                   as Text
import           Database.PostgreSQL.Simple           (ConnectInfo (..), Connection)
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import qualified Database.PostgreSQL.Simple.Types     as PG
import qualified GHC.IO.Exception                     as Exception

import qualified Tuttifrutti.Log                      as Log
import qualified Tuttifrutti.Log.Handle               as Log
import           Tuttifrutti.Pool                     (Pool)
import qualified Tuttifrutti.Pool                     as Pool
import           Tuttifrutti.Prelude

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  -- exponential backoff starting at 0.1 second
  Retry.exponentialBackoff (round @Double 0.1e6)
    -- with overall timeout of 10 minutes
    & Retry.limitRetriesByCumulativeDelay (round @Double 60e7)

type MonadPostgres env m =
  ( MonadReader env m
  , Has Handle env
  , MonadIO m
  )

data Config = Config
  { configRetryPolicy    :: RetryPolicy
    -- ^ policy for retrying failed connection attempts
  , configPoolConfig     :: Pool.Config
    -- ^ configuration for the connection pool
  , configConnectInfo    :: ConnectInfo
    -- ^ connection configuration
  , configConnectionInit :: Connection -> IO ()
    -- ^ A function that will be run for each newly opened 'Connection'.
    --   In postgresql TCP connection corresponts to a "session", thus
    --   this callback can be handidly used to e.g. @SET@ting some sesion
    --   configuration parameters.
  }

newtype Handle = Handle
  { handlePool :: Pool Connection }

newHandle
  :: Log.Handle
  -> Config
  -> IO Handle
newHandle logHandle config = do
  handlePool <- newConnectionPool logHandle config
  pure Handle{..}

closeHandle :: Handle -> IO ()
closeHandle Handle{..} =
  Pool.destroyAllResources handlePool

withConnection :: (MonadPostgres env m, MonadUnliftIO m) => (Connection -> m a) -> m a
withConnection f = do
  Handle{..} <- asks Has.getter
  Pool.withResource handlePool f

newConnectionPool
  :: Log.Handle
  -> Config
  -> IO (Pool Connection)
newConnectionPool logHandle config@Config{..} =
  Pool.createPool
    (connect logHandle config)
    disconnect
    configPoolConfig

connect :: Log.Handle -> Config -> IO Connection
connect logHandle Config{..} =
  Retry.recovering configRetryPolicy errorHandlers $ \Retry.RetryStatus{..} -> do
    when (rsIterNumber > 0) $ do
      with logHandle $ do
        Log.logInfo "Retrying to connect to database (${iter_number})"
          [ "iter_number" .= rsIterNumber
          , "cumulative_delay" .= rsCumulativeDelay
          , "previous_delay" .= rsPreviousDelay
          ]
    conn <- PG.connect configConnectInfo
    conn <$ configConnectionInit conn
  where
    errorHandlers =
      [ \_retryStatus -> Handler $ \Exception.IOError{..} -> do
          with logHandle $ do
            Log.logError "An IOError occurred in ${location} when connecting to postgres."
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

disconnect :: Connection -> IO ()
disconnect = PG.close


-- | Given the same kind of mapping as 'fromEnumField' outputs an
--   'Action' that encodes enum value.
toEnumField :: (a -> Text) -> a -> PG.Action
toEnumField mappingFn = PG.toField . mappingFn

-- | Parse an enum field given a postgresql type name (as appears in @pg_type@ table)
--   And a mapping function that converts enum values to their textual DB representation.
--
--   Example postgresql type:
--
--   @
--   CREATE TYPE election.language_ration AS ENUM
--     ( 'FI', 'SV', 'FI_SV', 'SV_FI' );
--   @
--
--   Which corresponds to Haskell Enum:
--
--   @
--   data LanguageRation = FI | SV | FI_SV | SV_FI
--     deriving (Eq, Ord, Enum, Bounded, Generic, Data)
--   @
--
--   And can be parsed with:
--
--   @
--   fromEnumField "language_ration"
--     \case FI    -> "FI"
--           SV    -> "SV"
--           FI_SV -> "FI_SV"
--           SV_FI -> "SV_FI"
--   @
fromEnumField
  :: (Typeable a, Enum a, Bounded a)
  => ByteString  -- ^ expected @pg_type@
  -> (a -> Text) -- ^ enum mapping
  -> PG.FieldParser a
fromEnumField pg_type mappingFn field_ maybeValue = do
  columnType <- PG.typename field_
  if columnType /= pg_type
    then PG.returnError PG.Incompatible field_ $ "expected a " <> show pg_type
    else do
      case maybeValue of
        Nothing -> do
          PG.returnError PG.UnexpectedNull field_ "null not expected, use 'optionalField'"
        Just value ->
          case lookup (Text.decodeUtf8 value) mappingTable of
            Nothing -> do
              PG.returnError PG.ConversionFailed field_
                ("Unmatched value: " <> show value <> ". Expected: " <> show (fst <$> mappingTable))
            Just a -> pure a
  where
    mappingTable =
      [ (mappingFn a, a) | a <- [minBound..maxBound] ]


-- | Like 'fromJSONField', but takes a `text` field and parses it
--   as if it's a 'Json.String'
--   Useful for newtypes, enums and such other typed strings.
fromJSONTextField :: (FromJSON a, Typeable a) => PG.FieldParser a
fromJSONTextField field_ maybeValue = do
  content :: Text <- PG.fromField field_ maybeValue
  case Json.fromJSON $ Json.String content of
    Json.Success a ->
      pure a
    Json.Error err ->
      PG.returnError PG.ConversionFailed field_
        ("fromJSON: " <> err)

-- | Like 'toJSONField', but tries to encode the json value into a
--   suitable postgresql type.
--
--   Objects are left out as @json@
--
--   Useful for newtypes, enums and such.
asJSONField :: ToJSON a => a -> PG.Action
asJSONField a =
  case toJSON a of
    Json.String s  -> PG.toField s
    Json.Null      -> PG.toField PG.Null
    Json.Array arr -> PG.toField (asJSONField <$> arr)
    Json.Bool b    -> PG.toField b
    Json.Number n  -> PG.toField n
    Json.Object o  -> PG.toField (Json.Object o)

