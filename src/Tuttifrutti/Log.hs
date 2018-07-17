module Tuttifrutti.Log
  ( MonadLog
  , localData
  , localDomain
  , logMessage, logMessageNow
  , logTrace, logTrace_
  , logInfo, logInfo_
  , logWarning, logWarning_
  , logError, logError_
  ) where


import           Tuttifrutti.Prelude

import qualified Data.Aeson             as Json
import qualified Data.Aeson.Types       as Json
import qualified Data.ByteString.Lazy   as LByteString
import qualified Data.Has               as Has
import qualified Data.Text.Encoding     as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Template     as Template
import qualified Data.Time              as Time
import qualified RIO.HashMap            as HashMap
import qualified System.Log.FastLogger  as FastLogger

import           Tuttifrutti.Log.Handle (Handle (..), LogEntry (..), LogSeverity (..))

type MonadLog env m =
  ( MonadReader env m
  , Has Handle env
  , MonadIO m
  )

-- | Extend the attached data of each log message in given action.
localData :: (MonadLog env m) => [Json.Pair] -> m a -> m a
localData as = local (Has.modifier extendHandle)
  where
    extendHandle :: Handle -> Handle
    extendHandle h =
      h { handleData = handleData h <> as }

-- | Extend the domain of each log message in given action.
localDomain :: (MonadLog env m) => Text -> m a -> m a
localDomain domain = local (Has.modifier extendHandle)
  where
    extendHandle :: Handle -> Handle
    extendHandle h =
      h { handleDomain = handleDomain h <> [ domain ] }

logMessage
  :: (MonadLog env m)
  => UTCTime -> LogSeverity -> Text -> [Json.Pair] -> m ()
logMessage t severity msg payload = do
  Handle{..} <- asks Has.getter
  liftIO $ FastLogger.pushLogStrLn handleLoggerSet $ handleFormat LogEntry
    { logEntryMessage   = interpolatedMessage msg $ Json.object payload
    , logEntryData      = handleData <> payload
    , logEntrySeverity  = severity
    , logEntryTimestamp = t
    , logEntryComponent = handleComponent
    , logEntryDomain    = handleDomain
    }

logMessageNow
  :: (MonadLog env m)
  => LogSeverity -> Text -> [Json.Pair] -> m ()
logMessageNow severity msg payload =
  liftIO Time.getCurrentTime >>= \now -> logMessage now severity msg payload

-- | Perform variable interpolation in a text using values from given json.
--   In case of any failures returns the message with placeholders left in.
--
--   The placeholder is marked with a dollar sign and (optionally) curly braces:
--
--   >>> interpolatedMessage "User ${cusno}" $ object [ "cusno" .= Just 1234 ]
--   "User 1234"
interpolatedMessage :: Text -> Json.Value -> Text
interpolatedMessage templateText payload =
  case Template.templateSafe templateText of
    Left _position -> templateText
    Right template -> LText.toStrict $ Template.render template context
   where
     context var = fromMaybe ("${" <> var <> "}") $ lookupJsonPath [var] payload

-- | Lookup and render a value under a nested path.
lookupJsonPath :: [Text] -> Json.Value -> Maybe Text
lookupJsonPath [] (Json.String s) = Just s
lookupJsonPath (var:path) (Json.Object o) =
  HashMap.lookup var o >>= lookupJsonPath path
lookupJsonPath _path json =
  Just $ Text.decodeUtf8 $ LByteString.toStrict $ Json.encode json

-- | Logging with attached data.
--
--   Our logging level policy:
--    - Trace     — Messages that are mostly useful for debugging,
--                  expose all details of what's going on,
--                  Might (and should) be a superfluous and contain
--                  potentially sensitive data.
--    - Info      — Messages that carry info about normal application
--                  flow. Looking at just these should provide a good
--                  picture of 'what just happened and why' without
--                  overwhelming the reader with internal details.
--    - Warning   — Messages about events that require attention of
--                  a human, but don't degrade the system.
--    - Error     — Errors. Reports of the things going wrong
--                  (not in the way that the system expected).
logWarning, logError, logInfo, logTrace
  :: (MonadLog env m) => Text -> [Json.Pair] -> m ()

logWarning msg payload = logMessageNow LogWarning msg payload
logError   msg payload = logMessageNow LogError   msg payload
logInfo    msg payload = logMessageNow LogInfo    msg payload
logTrace   msg payload = logMessageNow LogTrace   msg payload

logError_, logWarning_, logInfo_, logTrace_
  :: (MonadLog env m) => Text -> m ()

logError_   msg = logError   msg []
logWarning_ msg = logWarning msg []
logInfo_    msg = logInfo    msg []
logTrace_   msg = logTrace   msg []
