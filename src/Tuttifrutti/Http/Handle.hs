module Tuttifrutti.Http.Handle
  ( Handle(..)
  , newNetworkHandle
  , RequestId(..), addXRequestId
  , useVcrRecorder
  , httpRequestJson
  , httpResponseJson
  ) where

import           Tuttifrutti.Prelude

import           Control.Lens                (over)
import qualified Data.Aeson                  as Json
import qualified Data.ByteString.Lazy        as LByteString
import qualified Data.CaseInsensitive        as CaseInsensitive
import qualified Data.Has                    as Has
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.Encoding.Error    as Text.Error
import qualified Data.Time.Clock             as Time
import qualified Data.Vcr                    as Vcr
import           Network.HTTP.Client         (BodyReader, responseClose, responseOpen)
import qualified Network.HTTP.Client         as Http
import           Network.HTTP.Client.Conduit (Request, Response)
import qualified Network.HTTP.Client.Vcr     as Vcr
import qualified Network.HTTP.Types          as Http

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
          Log.logTrace "sending request" [ "request" .= httpRequestJson req ]
          requestedAt <- liftIO Time.getCurrentTime
          res <- liftIO $ responseOpen req manager
          respondedAt <- liftIO Time.getCurrentTime
          (res', bodyHead) <-liftIO $ do
            -- action that returns the next chunk of response body each time it's called
            let bodyReader :: IO ByteString = Http.responseBody res
            -- read 1KB of response for tracing purposes
            -- by doing that we mutate the response buffer
            bodyHead <- LByteString.toStrict <$> Http.brReadSome (Http.responseBody res) 1024
            -- here we reconstruct the buffer by making an action that will yield the head
            -- at the first invocation and just call original body reader subsequently
            bodyPrereader <- do
              bodyHeadVar <- newMVar bodyHead
              pure $ tryTakeMVar bodyHeadVar >>= fromMaybeM bodyReader
            pure (const bodyPrereader <$> res, bodyHead)
          Log.logTrace "response opened"
            [ "request" .= httpRequestJson req
            , "response" .= httpResponseJson (Text.decodeUtf8With Text.Error.ignore bodyHead <$ res)
            , "ttfb" .= respondedAt `Time.diffUTCTime` requestedAt
            ]
          pure res'
    , closeResponse_ = \logHandle res ->
        with logHandle $ Log.localDomain "http-client" $ do
          Log.logTrace_ "closing response"
          liftIO $ responseClose res
    }


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

-- | Json-encode http request, useful for logging.
httpRequestJson :: Http.Request -> Json.Value
httpRequestJson x = object
  [ "host"            .= show (Http.host x)
  , "port"            .= show (Http.port x)
  , "secure"          .= show (Http.secure x)
  , "requestHeaders"  .= headersObject (Http.requestHeaders x)
  , "path"            .= show (Http.path x)
  , "queryString"     .= show (Http.queryString x)
  , "method"          .= show (Http.method x)
  , "proxy"           .= show (Http.proxy x)
  , "redirectCount"   .= show (Http.redirectCount x)
  , "responseTimeout" .= show (Http.responseTimeout x)
  , "requestVersion"  .= show (Http.requestVersion x)
  ]

-- | Json-encode http response, useful for logging
httpResponseJson :: ToJSON a => Response a -> Json.Value
httpResponseJson response = object
  [ "status"  .= statusObject (Http.responseStatus response)
  , "headers" .= headersObject (Http.responseHeaders response)
  , "body"    .= Http.responseBody response
  ]

-- | Json-encode http status, useful for logging.
statusObject :: Http.Status -> Json.Value
statusObject (Http.Status code message) = object
  [ "code" .= code, "message" .= Text.decodeUtf8 message ]

-- | Json-encode http headers, useful for logging.
headersObject :: [Http.Header] -> Json.Value
headersObject =
  object . map (\(k,v) -> Text.decodeUtf8 (CaseInsensitive.original k) .= Text.decodeUtf8 v)

