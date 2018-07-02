module Tuttifrutti.Wai where

import           Tuttifrutti.Prelude

import qualified Data.Aeson.Types       as Json
import qualified Data.CaseInsensitive   as CI
import qualified Network.Wai            as Wai

import           Data.Text.Encoding     (decodeUtf8)
import qualified Tuttifrutti.Http       as Http
import qualified Tuttifrutti.Log        as Log
import qualified Tuttifrutti.Log.Handle as Log
import           Tuttifrutti.RequestId  (RequestId)
import qualified Tuttifrutti.RequestId  as RequestId

-- | A middleware that logs incoming http requests.
requestLogger
  :: Has Log.Handle env
  => env -> (RequestId -> Wai.Application) -> Wai.Application
requestLogger env =
  \app ->
   \req res -> do
      requestId <- do
        Wai.requestHeaders req
          & view RequestId.requestIdHeader
          & onNothing RequestId.random
      with env
        $ Log.localDomain "http-server"
        $ Log.logInfo "Incoming HTTP request: $method $path"
        $ requestJson req <> [ "request_id" .= requestId ]
      app requestId req res
  where
    requestJson :: Wai.Request -> [Json.Pair]
    requestJson req =
        [ "method"  .= (decodeUtf8 $ Wai.requestMethod req)
        , "path"    .= (decodeUtf8 $ Wai.rawPathInfo req)
        , "query"   .= (queryItemJson <$> Wai.queryString req)
        , "headers" .= object (headerJson <$> Wai.requestHeaders req)
        ]
      where
        queryItemJson (name, mValue) = (decodeUtf8 name, decodeUtf8 <$> mValue)
        headerJson (name, value) = decodeUtf8 (CI.original name) .= decodeUtf8 value

-- | Include request id everywhere where it' needed (logging, outgoing calls, etc)
withXRequestId
  :: ( MonadReader env m
     , Has Log.Handle env
     , Has Http.Handle env
     , MonadUnliftIO m
     )
  => RequestId -> m a -> m a
withXRequestId requestId =
  Log.localData [ "request_id" .= requestId ] -- add it to every logging statement
    . Http.withRequestId requestId            -- add it to every outgoing http call
