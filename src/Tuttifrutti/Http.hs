-- | Http service module.
--
--   Abstracts the way the http requests are performed to allow
--   mocking, caching and intercepting them without touching
--   the business code.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Tuttifrutti.Http
  ( module Network.HTTP.Simple
  , module Network.HTTP.Client.Conduit
  , module Network.HTTP.Client
  , MonadHttp
  , Handle
  , newNetworkHandle
  , bodyProducer
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
import           Network.HTTP.Simple         (setRequestQueryString)

import           Tuttifrutti.Http.Handle
import qualified Tuttifrutti.Log             as Log

-- | Constraint that captures the environments that are capable of performing
--   real or emulated http requests.
type MonadHttp env m =
  ( MonadIO m
  , MonadUnliftIO m
  , MonadReader env m
  , Has Handle env
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

withRequestId :: (MonadHttp env m) => RequestId -> m a -> m a
withRequestId = local . addXRequestId

withCassette
  :: (MonadHttp env m, MonadThrow m, MonadMask m)
  => FilePath -> m a -> m a
withCassette path m = do
  Vcr.withRecorder path $ \recorder -> local (useVcrRecorder recorder) m
