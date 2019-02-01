module Tuttifrutti.Env where

import qualified System.Envy         as Envy
import qualified Tuttifrutti.Log     as Log
import           Tuttifrutti.Prelude

-- | Decodes the configuration from environment variables.
--   Logs and throws in case of failure.
loadConfig :: (MonadIO m, Log.MonadLog env m, Envy.FromEnv a) => m a
loadConfig = do
  liftIO Envy.decodeEnv >>= onLeft
    (\err -> do
        Log.logError "Failed to load environment variable: ${error}"
          [ "error" .= err ]
        throwString err)

