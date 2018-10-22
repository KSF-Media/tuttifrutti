module Tuttifrutti.Http.Handle
  ( Handle(..)
  , newNetworkHandle
  , RequestId(..), addXRequestId
  , useVcrRecorder
  ) where

import           Tuttifrutti.Prelude

import           Control.Lens                (over)
import qualified Data.Has                    as Has
import qualified Data.Text.Encoding          as Text
import qualified Data.Time.Clock             as Time
import qualified Data.Vcr                    as Vcr
import           Network.HTTP.Client         (BodyReader, responseClose, responseOpen)
import qualified Network.HTTP.Client         as Http
import           Network.HTTP.Client.Conduit (Request, Response)
import qualified Network.HTTP.Client.Vcr     as Vcr
import qualified Network.HTTP.Types.Status   as Http

import           Tuttifrutti.Log             as Log
import qualified Tuttifrutti.Log.Handle      as Log
import           Tuttifrutti.RequestId       (RequestId (..), requestIdHeader)

-- | Primitives that has to be implemented in order to support 'MonadHttp'.
data Handle = Handle
  { openResponse_  :: Log.Handle -> Request -> IO (Response BodyReader)
  , closeResponse_ :: forall a. Log.Handle -> Response a -> IO ()
  }

-- | Handle that sends http requests over an actual real network.
--   With TLS/SSL support, keep-alives, gzip, and all other fanciness.
--
--   Useful for production and development alike.
--
--   'Manager' cleans up after itself so no bracketing is needed. Yay!
newNetworkHandle :: Http.ManagerSettings -> IO Handle
newNetworkHandle settings = do
  manager <- Http.newManager settings
  pure $ Handle
    { openResponse_ = \logHandle req ->
        with logHandle $ Log.localDomain "http-client" $ do
          Log.logTrace "sending request" $ reqInfo req
          requestedAt <- liftIO Time.getCurrentTime
          res <- liftIO $ responseOpen req manager
          respondedAt <- liftIO Time.getCurrentTime
          Log.logTrace "response opened" $ reqInfo req <> resInfo res <>
            [ "ttfb" .= respondedAt `Time.diffUTCTime` requestedAt ]
          pure res
    , closeResponse_ = \logHandle res ->
        with logHandle $ Log.localDomain "http-client" $ do
          Log.logTrace_ "closing response"
          liftIO $ responseClose res
    }
  where
    reqInfo req =
      [ "secure" .= Http.secure req
      , "path"   .= Text.decodeUtf8 (Http.path req)
      , "port"   .= Http.port req
      , "query"  .= Text.decodeUtf8 (Http.queryString req)
      ]
    resInfo res =
      [ "status" .= Http.statusCode (Http.responseStatus res) ]

-- | Make it so that every outgoing request contains the X-Request-ID header with a given value.
--   Wouldn't overwrite existing X-Request-ID header.
addXRequestId :: (Has Handle env) => RequestId -> env -> env
addXRequestId requestId = Has.modifier $ \h@Handle{..} -> h
  { openResponse_ = \logHandle rq ->
      openResponse_ logHandle rq
        { Http.requestHeaders =
            Http.requestHeaders rq
              & over requestIdHeader (Just . fromMaybe requestId)
         }
  }

useVcrRecorder :: (Has Handle env) => Vcr.Recorder -> env -> env
useVcrRecorder recorder = do
  Has.modifier $ \env -> env
    { openResponse_ = \logHandle httpRequest -> do
        request <- Vcr.fromHttpRequest httpRequest
        Vcr.responseOpen
          recorder Vcr.Always (Vcr.matchRequest request)
          (openResponse_ env logHandle) (closeResponse_ env logHandle)
          httpRequest
    }
