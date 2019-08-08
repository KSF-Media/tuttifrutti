-- | Http service module.
--
--   Abstracts the way the http requests are performed to allow
--   mocking, caching and intercepting them without touching
--   the business code.
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BlockArguments #-}
module Tuttifrutti.Http
  ( module Network.HTTP.Simple
  , module Network.HTTP.Client.Conduit
  , module Network.HTTP.Client
  , MonadHttp
  , Handle
  , newNetworkHandle
  , bodyProducer
  , jsonResponse
  , eitherJsonResponse
  , consumeJsonResponse
  , withHttpResponse
  , withHttpResponseC
  , RequestId(..)
  , withRequestId
  , withCassette
  ) where

import           Conduit
import           Tuttifrutti.Prelude

import qualified Data.Has                    as Has
import qualified Data.Vcr                    as Vcr
import           Network.HTTP.Client         (BodyReader, responseClose, responseOpen, withResponse)
import qualified Network.HTTP.Client         as Http
import           Network.HTTP.Client.Conduit (HasHttpManager (..), Request, Response,
                                              bodyReaderSource, defaultManagerSettings,
                                              parseRequest, responseStatus)
import           Network.HTTP.Simple         (setRequestQueryString, JSONException(..), HttpException(..))
import           Network.HTTP.Simple         as Http
import           Network.HTTP.Types          as Http
import qualified Data.ByteString.Lazy        as LByteString
import qualified Data.Conduit.Attoparsec     as Conduit
import qualified Data.Text.Encoding          as Text
import qualified Data.Aeson                  as Json
import qualified Data.Aeson.Types            as Json

import           Tuttifrutti.Http.Handle
import qualified Tuttifrutti.Log             as Log

#if !(MIN_VERSION_conduit(1,3,0))
type ConduitT = ConduitM
#endif

-- | Constraint that captures the environments that are capable of performing
--   real or emulated http requests.
type MonadHttp env m =
  ( MonadIO m
  , MonadUnliftIO m
  , MonadReader env m
  , Has Handle env
  , MonadThrow m
  )

-- | Takes http 'Request' and gives a producer that yields chuncked body.
--
--   'MonadResource' allows to register the 'closeResponse' action which
--   will be called as soon as possible (when the full body is read).
bodyProducer
  :: (MonadHttp env m, Log.MonadLog env m, MonadResource m)
  => Request
  -> ConduitT i ByteString m ()
bodyProducer request = do
  withHttpResponseC request (bodyReaderSource . Http.responseBody)

-- | Perform an HTTP 'Request' and consume the body as JSON. See 'consumeJsonResponse' for details.
--   Would throw 'JSONException' if the decoding/parsing fails.
jsonResponse
  :: (MonadHttp env m, Log.MonadLog env m, FromJSON a)
  => Request
  -> m (Response a)
jsonResponse req =
  eitherJsonResponse req >>= traverse (either throwM pure)

-- | Perform an HTTP 'Request' and consume the body as JSON. See 'consumeJsonResponse' for details.
eitherJsonResponse
  :: (MonadHttp env m, Log.MonadLog env m, FromJSON a)
  => Request
  -> m (Response (Either JSONException a))
eitherJsonResponse request = do
  let jsonRequest = request & Http.addRequestHeader Http.hAccept "application/json"
  withHttpResponse jsonRequest $ consumeJsonResponse jsonRequest

-- | Takes the 'Request' and an already opened 'Response'. Consumes the body of the request
--   parse it as JSON and if that succeeds parses that JSON using 'FromJSON' instance.
--   Returns the 'Response' with either an 'JSONException' or parsed value.
consumeJsonResponse
  :: (Log.MonadLog env m, MonadHttp env m, FromJSON a)
  => Request
  -> Response BodyReader
  -> m (Response (Either JSONException a))
consumeJsonResponse request response = runConduitRes $ for response $ \bodyReader -> do
  (decodedBody, body) <- bodyReaderSource bodyReader .| getZipSink do
    parsed <- ZipSink $ Conduit.sinkParserEither Json.json
    body   <- ZipSink sinkLazy
    pure (parsed, body)
  case decodedBody of
    Left err -> do
      Log.logError "Failed to decode HTTP response body as JSON: ${error}"
        [ "error" .= show err
        , "body"  .= Text.decodeUtf8 (LByteString.toStrict body)
        ]
      pure $ Left $ JSONParseException request (() <$ response) err
    Right json ->
      case Json.parseEither parseJSON json of
        Left err -> do
          Log.logError "Failed to parse JSON response: ${error}"
            [ "error" .= show err
            , "json"  .= json
            ]
          pure $ Left $ JSONConversionException request (json <$ response) err
        Right a -> pure $ Right a

-- | Takes http 'Request' performs it and passes the 'Response' to a provided callback.
--   Once the callback is done the 'Response' will be closed.
withHttpResponse
  :: (MonadHttp env m, Log.MonadLog env m)
  => Request
  -> (Response BodyReader -> m a)
  -> m a
withHttpResponse req =
  bracket (openResponse req) closeResponse

withHttpResponseC
  :: (MonadResource m, Log.MonadLog env m, MonadHttp env m)
  => Request
  -> (Response BodyReader -> ConduitT i o m a)
  -> ConduitT i o m a
withHttpResponseC request responseHandler = do
  Handle{..} <- asks Has.getter
  logHandle <- lift $ asks Has.getter
  bracketP
    (openResponse_ logHandle request)
    (closeResponse_ logHandle)
    responseHandler

-- | Takes http 'Request' and returns an opened response.
--
--   It's the responsibility of the caller to call 'closeResponse' once
--   they finished consuming this 'Response'.
openResponse
  :: (MonadHttp env m, Log.MonadLog env m)
  => Request -> m (Response BodyReader)
openResponse request = do
  Handle{..} <- asks Has.getter
  logHandle <- asks Has.getter
  liftIO $ openResponse_ logHandle request

-- | Takes http 'Response' and does the duties of freeing the resources
---  that are used by it.
closeResponse
  :: (MonadHttp env m, Log.MonadLog env m)
  => Response a -> m ()
closeResponse response = do
  Handle{..} <- asks Has.getter
  logHandle <- asks Has.getter
  liftIO $ closeResponse_ logHandle response

withRequestId :: (Has Handle env, MonadReader env m) => RequestId -> m a -> m a
withRequestId = local . addXRequestId

withCassette
  :: (MonadHttp env m, MonadMask m)
  => FilePath -> m a -> m a
withCassette path m = do
  Vcr.withRecorder path $ \recorder -> local (useVcrRecorder recorder) m
