module Tuttifrutti.Http.Handle
  ( Handle(..)
  , newNetworkHandle
  , RequestId(..), addXRequestId
  , useVcrRecorder
  ) where

import           Tuttifrutti.Prelude

import qualified Data.Has                    as Has
import qualified Data.Text.Encoding          as Text
import qualified Data.Vcr                    as Vcr
import           Network.HTTP.Client         (BodyReader, responseClose, responseOpen)
import qualified Network.HTTP.Client         as Http
import           Network.HTTP.Client.Conduit (Request, Response)
import qualified Network.HTTP.Client.Vcr     as Vcr
import qualified Network.HTTP.Types.Status   as Http


import           Tuttifrutti.Log             as Log
import qualified Tuttifrutti.Log.Handle      as Log

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
          res <- liftIO $ responseOpen req manager
          Log.logTrace "response opened" $ reqInfo req <> resInfo res
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

newtype RequestId = RequestId Text
  deriving (Show)

-- | Make it so that every outgoing request contains the X-Request-ID header with a given value.
addXRequestId :: (Has Handle env) => RequestId -> env -> env
addXRequestId (RequestId requestId) = Has.modifier $ \h@Handle{..} -> h
  { openResponse_ = \logHandle rq ->
      openResponse_ logHandle rq
        { Http.requestHeaders =
            if isJust $ lookup "X-Request-ID" $ Http.requestHeaders rq
            then Http.requestHeaders rq
            else Http.requestHeaders rq <> [("X-Request-ID", Text.encodeUtf8 requestId)]
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
