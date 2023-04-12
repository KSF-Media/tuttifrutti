-- | Wrappers for Servant.Client
--
--   We need to wrap some Servant.Client functions so we can
--   log requests and pass the X-Request-Id header forward.

module Tuttifrutti.Servant.Client
  ( createLoggingManager
  , wrapClientEnv
  ) where

import Tuttifrutti.Prelude

import           Data.Aeson.Key         (toText)
import           Servant.Client         (ClientEnv(..))
import           Servant.Client.Core    (addHeader)
import           Tuttifrutti.Log        as Log
import           Tuttifrutti.Log.Handle as Log
import qualified Data.Aeson as Aeson
import qualified Tuttifrutti.Http       as Http
import qualified Tuttifrutti.RequestId  as RequestId

-- | Create a manager which logs all requests using the specified logger,
--   adding request_id to the log message from the request headers.
createLoggingManager :: MonadIO m => Text -> Text -> Log.Handle -> Http.ManagerSettings -> m Http.Manager
createLoggingManager fromService toService logHandle settings =
  Http.newManagerSettings $ settings
          -- The function 'managerModifyRequest' would be the best-looking function
          -- for this, but it's executed multiple times per request
          -- https://github.com/snoyberg/http-client/issues/350
          { Http.managerWrapException = \r e -> do
                -- it would be super nice to grab the request id from anything else,
                -- but this is all we have in this context
                let requestId = decodeUtf8Lenient . snd <$> find (\(h, _) -> h == "X-Request-Id") (Http.requestHeaders r)
                with logHandle $
                    Log.logInfo (fromService <> " -> " <> toService <> " request")
                       [ "url" .= decodeUtf8Lenient (Http.path r)
                       , "method" .= decodeUtf8Lenient (Http.method r)
                       , "request_id" .= fromMaybe "[none]" requestId
                       , "from" .= fromService
                       , "to" .= toService
                       ]
                Http.managerWrapException Http.defaultManagerSettings r e
          }

unwrapAesonString :: Aeson.Value -> Maybe Text
unwrapAesonString (Aeson.String t) = Just t
unwrapAesonString _ = Nothing

-- | Wrap a ClientEnv so that it adds the X-Request-ID header based on
--   the current requestId in the logger
wrapClientEnv :: MonadIO m => ClientEnv -> Log.Handle -> m ClientEnv
wrapClientEnv ClientEnv{..} logger = do
  requestId <- fromMaybeM ((\ (RequestId.RequestId t) -> t) <$> RequestId.random)
    $ find (\(k, _) -> toText k == "request_id") (Log.handleData logger) >>= unwrapAesonString . snd

  pure ClientEnv
        { makeClientRequest = \url request -> makeClientRequest url (addHeader "X-Request-ID" requestId request)
        , ..
        }
