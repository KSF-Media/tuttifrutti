module Tuttifrutti.Log.Handle
  ( LogEntry(..), LogSeverity(..)
  , Handle(..)
  , newHandle, waitHandle, closeHandle
  , newStdoutHandle, newFileHandle
  , googleMessage, devMessage
  ) where

import           Tuttifrutti.Prelude

import qualified Data.Aeson               as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Aeson.Types         as Json
import qualified Data.Text                as Text
import qualified Data.Text.Lazy.Builder   as Text.Builder
import qualified Data.Time                as Time
import qualified System.Log.FastLogger    as FastLogger

-- some iso8601-related stuff, when we switch to time-1.9 this can be replaced
-- with just an import of 'Data.Time.Format.ISO8601'

iso8601Format :: String
iso8601Format = Time.iso8601DateFormat (Just "%H:%M:%S%z")

formatShow :: Time.FormatTime t => String -> t -> String
formatShow = Time.formatTime Time.defaultTimeLocale

iso8601Show :: Time.FormatTime t => t -> String
iso8601Show = formatShow iso8601Format

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

data Handle = Handle
  { handleLoggerSet :: FastLogger.LoggerSet
  , handleFormat    :: LogEntry -> FastLogger.LogStr
  , handleComponent :: Text
  , handleDomain    :: [Text]
  , handleData      :: [Json.Pair]
  }

newHandle :: (LogEntry -> FastLogger.LogStr) -> Text -> FastLogger.LoggerSet -> Handle
newHandle handleFormat handleComponent handleLoggerSet = Handle{..}
  where
    handleDomain    = []
    handleData      = []

newStdoutHandle
  :: Text
  -> (LogEntry -> FastLogger.LogStr)
  -> IO Handle
newStdoutHandle handleComponent handleFormat =
  newHandle handleFormat handleComponent <$> do
    -- setup an async logger, when the main thread is killed 'closeHandle'
    -- must be called to ensure that everything has been written
    FastLogger.newStdoutLoggerSet FastLogger.defaultBufSize

newFileHandle
  :: FilePath
  -> Text
  -> (LogEntry -> FastLogger.LogStr)
  -> IO Handle
newFileHandle path handleComponent formatLogEntry = do
  newHandle formatLogEntry handleComponent
    <$> FastLogger.newFileLoggerSet FastLogger.defaultBufSize path


-- | Print json logs to Stackdriver.
--   The structure we should use is quite undocumented,
--   but on the internet you can find traces on how to do it, e.g. here:
--   https://github.com/GoogleCloudPlatform/fluent-plugin-google-cloud/issues/99
googleMessage :: LogEntry -> FastLogger.LogStr
googleMessage LogEntry{..} =
  FastLogger.toLogStr $ Json.encode $ Json.object
    [ "message" .= logEntryMessage
    , "timestamp" .= iso8601Show logEntryTimestamp
    , "severity" .= case logEntrySeverity of
                      LogInfo    -> "INFO"    :: Text
                      LogTrace   -> "DEBUG"   :: Text
                      LogWarning -> "WARNING" :: Text
                      LogError   -> "ERROR"   :: Text
    , "context" .= Text.intercalate "/" (logEntryComponent : logEntryDomain)
    , "msgData" .= Json.object logEntryData
    ]
  where

devMessage :: LogEntry -> FastLogger.LogStr
devMessage LogEntry{..} =
  FastLogger.toLogStr $ Text.Builder.toLazyText $ mconcat
    [ Text.Builder.fromString $ iso8601Show logEntryTimestamp
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

-- | Close the handle. Flushes the logging output and permanently closes it.
closeHandle :: Handle -> IO ()
closeHandle Handle{..} = do
  FastLogger.rmLoggerSet handleLoggerSet

-- | Wait for handle. Flushes the logging output waits till it's flushed.
waitHandle :: Handle -> IO ()
waitHandle Handle{..} = do
  FastLogger.flushLogStr handleLoggerSet
