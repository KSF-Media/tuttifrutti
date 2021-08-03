module Tuttifrutti.Sentry where

import           Tuttifrutti.Prelude

import           Data.Aeson                             (Value, withObject,
                                                         (.:))

import qualified Data.Text                              as Text
import qualified System.Log.Raven                       as Sentry
import qualified System.Log.Raven.Transport.HttpConduit as Sentry
import           System.Log.Raven.Types                 (SentryLevel (..))

import qualified Tuttifrutti.Log.Handle                 as Log

newtype Handle = Handle { logSentry :: SentryLevel -> Text -> [(Text, Value)]  -> IO () }

data Config = Config
  { configEnvironment :: String
  , configDsn         :: String
  }

instance FromJSON Config where
  parseJSON = withObject "SentryConfig" $ \v ->
    Config
      <$> v .: "environment"
      <*> v .: "dsn"

initSentry
  :: String
  -> [(String, String)]
  -> String
  -> SentryLevel
  -> Text
  -> [(Text, Value)]
  -> IO ()
initSentry dsn initialTags serviceName level logMessage extraTags = do
  sentry <- Sentry.initRaven dsn (Sentry.tags initialTags) Sentry.sendRecord Sentry.stderrFallback
  Sentry.register sentry serviceName level (Text.unpack logMessage) (Sentry.extra $ map (first Text.unpack) extraTags)

disabledHandle :: SentryLevel -> Text -> [(Text, Value)] -> IO ()
disabledHandle _ _ _ = pure ()

toSentryLevel :: Log.LogSeverity -> SentryLevel
toSentryLevel = \case
  Log.LogTrace   -> Debug
  Log.LogInfo    -> Info
  Log.LogWarning -> Warning
  Log.LogError   -> Error
