module Tuttifrutti.Log.Handle
  ( LogEntry(..), LogSeverity(..)
  , Handle(..)
  , newHandle, waitHandle, closeHandle
  , newStdoutHandle, newFileHandle, newDirectoryHandle
  , googleMessage, devMessage, devMessageCompact
  ) where

import           Tuttifrutti.Prelude

import qualified Data.Aeson                 as Json
import qualified Data.Aeson.Encode.Pretty   as Json
import qualified Data.Aeson.Text            as Json
import qualified Data.Aeson.Types           as Json
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Builder     as Text.Builder
import qualified Data.Time.Format.ISO8601   as Time
import           System.Directory           (createDirectoryIfMissing)
import qualified System.IO                  as IO
import qualified System.IO.Temp             as Temp
import qualified System.Log.FastLogger      as FastLogger


data LogSeverity = LogTrace | LogInfo | LogWarning | LogError
  deriving (Show, Eq, Ord)

data LogEntry = LogEntry
  { logEntryMessage   :: Text
  , logEntryData      :: [Json.Pair]
  , logEntryComponent :: Text
  , logEntryDomain    :: [Text]
  , logEntrySeverity  :: LogSeverity
  , logEntryTimestamp :: UTCTime
  } deriving (Show, Eq)

type Formatter = LogEntry -> FastLogger.LogStr

data Handle = Handle
  { handleLoggerSet :: FastLogger.LoggerSet
  , handleFormat    :: Formatter
  , handleComponent :: Text
  , handleDomain    :: [Text]
  , handleData      :: [Json.Pair]
  }

newHandle :: Formatter -> Text -> FastLogger.LoggerSet -> Handle
newHandle handleFormat handleComponent handleLoggerSet = Handle{..}
  where
    handleDomain    = []
    handleData      = []

newStdoutHandle
  :: Text
  -> (Formatter)
  -> IO Handle
newStdoutHandle handleComponent handleFormat =
  newHandle handleFormat handleComponent <$> do
    -- setup an async logger, when the main thread is killed 'closeHandle'
    -- must be called to ensure that everything has been written
    FastLogger.newStdoutLoggerSet FastLogger.defaultBufSize

newFileHandle
  :: FilePath
  -> Text
  -> Formatter
  -> IO Handle
newFileHandle path handleComponent formatLogEntry = do
  newHandle formatLogEntry handleComponent
    <$> FastLogger.newFileLoggerSet FastLogger.defaultBufSize path

-- | Writes to a file in a specified directory with a randomized name
--
--   >>> newDirectoryHandle "logs" "test.log" "myapp"
--   Logs are written to logs/test123.log
newDirectoryHandle
  :: FilePath -- ^ directory in which to create a file
  -> String   -- ^ template for the name (@foo.log@ -> @foo123.log@)
  -> Text     -- ^ logging component name
  -> IO Handle
newDirectoryHandle dir template handleComponent = do
  createDirectoryIfMissing True "logs"
  logPath <- Temp.emptyTempFile dir template
  logHandle <- newFileHandle logPath handleComponent devMessage
  IO.putStrLn $ "Logs are written to " <> logPath
  pure logHandle

-- | Print json logs to Stackdriver.
--   The structure we should use is quite undocumented,
--   but on the internet you can find traces on how to do it, e.g. here:
--   https://github.com/GoogleCloudPlatform/fluent-plugin-google-cloud/issues/99
googleMessage :: Formatter
googleMessage LogEntry{..} =
  FastLogger.toLogStr $ Json.encode $ Json.object
    [ "message" .= logEntryMessage
    , "timestamp" .= Time.iso8601Show logEntryTimestamp
    , "severity" .= case logEntrySeverity of
                      LogInfo    -> "INFO"    :: Text
                      LogTrace   -> "DEBUG"   :: Text
                      LogWarning -> "WARNING" :: Text
                      LogError   -> "ERROR"   :: Text
    , "context" .= Text.intercalate "/" (logEntryComponent : logEntryDomain)
    , "msgData" .= Json.object logEntryData
    ]
  where

devMessage :: Formatter
devMessage LogEntry{..} =
  FastLogger.toLogStr $ Text.Builder.toLazyText $ mconcat
    [ Text.Builder.fromString $ Time.iso8601Show logEntryTimestamp
    , ": "
    , case logEntrySeverity of
        LogInfo    -> "INFO"
        LogTrace   -> "DEBUG"
        LogWarning -> "WARNING"
        LogError   -> "ERROR"
    , " "
    , Text.Builder.fromText
        $ Text.intercalate "/" (logEntryComponent : logEntryDomain)
    , " — "
    , Text.Builder.fromText logEntryMessage
    , " "
    , Json.encodePrettyToTextBuilder $ Json.object logEntryData
    ]

devMessageCompact :: Formatter
devMessageCompact LogEntry{..} =
  FastLogger.toLogStr $ Text.Builder.toLazyText $ mconcat
    [ Text.Builder.fromString $ Time.iso8601Show logEntryTimestamp
    , ": "
    , case logEntrySeverity of
        LogInfo    -> "INFO"
        LogTrace   -> "DEBUG"
        LogWarning -> "WARNING"
        LogError   -> "ERROR"
    , " "
    , Text.Builder.fromText
        $ Text.intercalate "/" (logEntryComponent : logEntryDomain)
    , " — "
    , Text.Builder.fromText logEntryMessage
    , " "
    , Json.encodeToTextBuilder $ Json.object logEntryData
    ]

-- | Close the handle. Flushes the logging output and permanently closes it.
closeHandle :: Handle -> IO ()
closeHandle Handle{..} = do
  FastLogger.rmLoggerSet handleLoggerSet

-- | Wait for handle. Flushes the logging output waits till it's flushed.
waitHandle :: Handle -> IO ()
waitHandle Handle{..} = do
  FastLogger.flushLogStr handleLoggerSet
