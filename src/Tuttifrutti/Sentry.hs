module Tuttifrutti.Sentry where

import           Tuttifrutti.Prelude

import           Data.Aeson             (Value, withObject, (.:))
import           Data.Aeson.Types       (Pair)

import qualified Data.Text              as Text
import           System.Log.Raven.Types (SentryLevel (..), SentryRecord)

import qualified Tuttifrutti.Log.Handle as Log

newtype Handle = Handle { logSentry :: SentryLevel -> String -> (SentryRecord -> SentryRecord) -> IO () }

data Config = Config
  { configEnvironment :: String
  , configDsn         :: String
  }

instance FromJSON Config where
  parseJSON = withObject "SentryConfig" $ \v ->
    Config
      <$> v .: "environment"
      <*> v .: "dsn"

toSentryLevel :: Log.LogSeverity -> SentryLevel
toSentryLevel = \case
  Log.LogTrace   -> Debug
  Log.LogInfo    -> Info
  Log.LogWarning -> Warning
  Log.LogError   -> Error

toSentryParams :: [Pair] -> [(String, Value)]
toSentryParams = map (first Text.unpack)
