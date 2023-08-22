{-# OPTIONS_GHC -Wno-deprecations #-} -- Ignore deprecation of Wai.requestBody
module Tuttifrutti.Wai where

import           Tuttifrutti.Prelude

import qualified Data.Aeson.Key              as Key
import qualified Data.Aeson.Types            as Json
import qualified Data.ByteString.Char8       as ByteString8
import qualified Data.ByteString.Lazy        as LByteString
import qualified Data.CaseInsensitive        as CI
import qualified Data.List                   as List
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8)
import qualified Network.HTTP.Types          as Http
import qualified Network.Socket              as Socket
import qualified Network.URI                 as URI
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Middleware.Cors as Wai

import qualified Tuttifrutti.Http            as Http
import qualified Tuttifrutti.Log             as Log
import qualified Tuttifrutti.Log.Handle      as Log
import           Tuttifrutti.RequestId       (RequestId)
import qualified Tuttifrutti.RequestId       as RequestId

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
          & fromMaybeM RequestId.random
      with env
        $ Log.localDomain "http-server"
        $ Log.logInfo "Incoming HTTP request: $method $path"
        $ requestToJSON req <> [ "request_id" .= requestId ]
      app requestId req res

requestToJSON :: Wai.Request -> [Json.Pair]
requestToJSON request =
    [ "version" .= show (Wai.httpVersion request)
    , "secure"  .= Wai.isSecure request
    , "host"    .= sockAddrToJSON (Wai.remoteHost request)
    , "method"  .= (decodeUtf8 $ Wai.requestMethod request)
    , "path"    .= (decodeUtf8 $ Wai.rawPathInfo request)
    , "query"   .= object (queryItemJson <$> Wai.queryString request)
    , "headers" .= object (headerJson <$> Wai.requestHeaders request)
    ]
  where
    queryItemJson (name, mValue) = Key.fromText (decodeUtf8 name) .= fmap decodeUtf8 mValue
    headerJson (name, value) = Key.fromText (decodeUtf8 (CI.original name)) .= decodeUtf8 value

sockAddrToJSON :: Socket.SockAddr -> Json.Value
sockAddrToJSON = object . \case
  Socket.SockAddrInet port (Socket.hostAddressToTuple -> host) ->
    [ "host" .= host
    , "port" .= toInteger port
    ]
  Socket.SockAddrInet6 port flow (Socket.hostAddress6ToTuple -> host) scope ->
    [ "host" .= host
    , "port" .= toInteger port
    , "flow" .= flow
    , "scope" .= scope
    ]
  Socket.SockAddrUnix path -> [ "unix" .= toJSON path ]

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

-- | A rule against a given domain can be checked using 'allowDomain'.
data AllowDomain
  = -- | allows origin iff it has exactly the same domain
    AllowDomain Text
    -- | allows origin iff its domain ends as specified
  | AllowDomainSuffix Text
  deriving (Show, Eq, Ord, Data, Generic)

-- | Run a given domain through 'AllowDomain' check.
allowDomain :: AllowDomain -> Text -> Bool
allowDomain (AllowDomain allowed)             = (== allowed)
allowDomain (AllowDomainSuffix allowedSuffix) = Text.isSuffixOf allowedSuffix

-- | Extracts the Origin domain from the requests and runs it through
--   given 'AllowDomain' checks.
--   If any of the checks succeeds returns the 'Wai.Origin',
--   otherwise return 'Nothing'.
checkRequestOrigin :: [AllowDomain] -> Wai.Request -> Maybe Wai.Origin
checkRequestOrigin allowDomains request = do
  let headers = Wai.requestHeaders request
  origin <- lookup "Origin" headers
  uri <- URI.parseURI $ ByteString8.unpack origin
  authority <- URI.uriAuthority uri
  let domain = Text.pack $ URI.uriRegName authority
  guard $ any (`allowDomain` domain) allowDomains
  pure origin

-- | Body of 'Wai.Request' is fetched by repeatedly calling @requestBody :: Wai.Request -> IO ByteString@,
--   which reads the next chunk from the socket. So consuming it is possible only once from a given @Wai.Request@.
--
--   This utility reads the body out and returns it as a single bytestring along with a copy of a request whose
--   'Wai.requestBody' would also return that bytestring as a single chunk, which allows to access the body
--   from several places to which the request is being passed.
accessStrictRequestBody :: MonadIO m => Wai.Request -> m (LByteString, Wai.Request)
accessStrictRequestBody request = do
  body <- liftIO $ Wai.strictRequestBody request
  (body,) <$> setRequestBody body request

setRequestBody :: MonadIO m => LByteString -> Wai.Request -> m Wai.Request
setRequestBody body request = do
  bodyVar <- newMVar body
  pure request
    { Wai.requestBody = maybe mempty LByteString.toStrict <$> tryTakeMVar bodyVar
    , Wai.requestBodyLength = Wai.KnownLength $ fromInteger $ toInteger $ LByteString.length body
    }

setRequestQuery :: Http.Query -> Wai.Request -> Wai.Request
setRequestQuery query request = request
  { Wai.queryString = query
  , Wai.rawQueryString = Http.renderQuery True query
  }

setRequestMethod :: Http.Method -> Wai.Request -> Wai.Request
setRequestMethod method request = request
  { Wai.requestMethod = method }

setRequestPath :: [Text] -> Wai.Request -> Wai.Request
setRequestPath path request = request
  { Wai.pathInfo = path
  , Wai.rawPathInfo = foldMap encodeUtf8 $ List.intersperse "/" path
  }

removeRequestHeader :: Http.HeaderName -> Wai.Request -> Wai.Request
removeRequestHeader headerName request = request
  { Wai.requestHeaders = Wai.requestHeaders request
      & filter (\(name, _value) -> name /= headerName)
  }

-- | Takes the request consumes the body, and returns two request copies
--   from which that body can be read. The body of the original request
--   can't be read after this.
cloneRequest :: MonadIO m => Wai.Request -> m (Wai.Request, Wai.Request)
cloneRequest request = do
  body <- liftIO $ Wai.strictRequestBody request
  (,) <$> setRequestBody body request <*> setRequestBody body request
