{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
-- | Tuttifrutti's adapter to servant.
--
--   As it is today, servant is very extensible, but not very configurable.
--   Configuration thus have to be done by extending existing combinators.
--
--   This module is designated to be Persona's entry point to servant. It reexports
--   things that are often needed, defines new ones and redefines some of existing
--   to behave as we want.
--
--   There are several reasons to resort to shadowing of servant's APIs:
--
--   - to avoid making combinators with new names that have very little difference
--     from the ones that servant supplies (it's confusing to e.g. have both 'JSON'
--     and 'PersonaJSON' in the scope)
--   - to provide simpler migration path for when we won't need these combinators anymore.
--
--   Servant really should provide ways to configure the server combinators directly,
--   but it's a work-in-progress. The following issues are tracking the stuff that's
--   relevant to this module:
--
--   - https://github.com/haskell-servant/servant/issues/685
--   - https://github.com/haskell-servant/servant/issues/689
--   - https://github.com/haskell-servant/servant/issues/732
module Tuttifrutti.Servant
  ( module Servant
  , module Tuttifrutti.Servant
  ) where

import           Tuttifrutti.Prelude

import qualified Data.ByteString.Lazy                       as LByteString
import           Data.Swagger                               as Swagger
import qualified Data.Text                                  as Text
import qualified Network.HTTP.Types.Header                  as Http
import qualified Network.Wai                                as Wai
import           Servant.Swagger                            as Swagger

import           Data.Extensible                            (emptyRecord, (<:), (@=))
import           Servant                                    as Servant hiding (JSON)
import qualified Servant
import           Servant.API.ContentTypes                   (canHandleCTypeH)
import           Servant.Server.Internal.RoutingApplication (addBodyCheck, delayedFailFatal,
                                                             withRequest)

import qualified Tuttifrutti.Error as Error


-- | This 'JSON' type shadows the one from servant. It's purpose is to label 'application/json'
--   and associated behaviours.
data JSON

-- | The content-type string is the same as provided by Servant (application/json;charset=utf-8).
instance Servant.Accept JSON where
  contentType  _ = Servant.contentType  (Proxy :: Proxy Servant.JSON)
  contentTypes _ = Servant.contentTypes (Proxy :: Proxy Servant.JSON)

instance Servant.MimeRender Servant.JSON a => Servant.MimeRender JSON a where
  mimeRender _ = Servant.mimeRender (Proxy :: Proxy Servant.JSON)

instance Servant.MimeUnrender Servant.JSON a => Servant.MimeUnrender JSON a where
  mimeUnrender _ = Servant.mimeUnrender (Proxy :: Proxy Servant.JSON)

data InvalidRequest
    -- ^ Error returned by aeson when we tried to parse json body
  = InvalidJson String
  deriving (Show, Eq, Ord, Generic, Typeable)

instance ToSchema InvalidRequest

instance ToJSON InvalidRequest where
  toJSON (InvalidJson err) =
    object [ "errors" .= object [ "body" .= ("invalid json: " <> err) ]]

-- | Combinator like 'ReqBody', but:
--
--   - accepted content-type is hardcoded to faro's 'JSON'
--   - 'Unjson' is used for decoding
--   - when decoding fails the error response is returned as json
--     with appropriate content-type header
data JsonReqBody (a :: Type)

instance
  (HasSwagger api, ToSchema a)
  => Swagger.HasSwagger (JsonReqBody a :> api) where
  toSwagger _ =
    toSwagger (Proxy @(ReqBody '[JSON] a :> api))
      & Error.declareErrorResponse (Proxy @"invalid_request_body")
      & Error.declareErrorResponse (Proxy @"unsupported_media_type")

-- TODO: This definition is based on the 'HasServer ReqBody' instance from
-- servant-server-0.12. Once we switch to servant-server-0.13 it should be
-- possible to express this instance in terms of lenient 'ReqBody'.
--
-- Ideally, this should not be needed at all once (as the rest of this module)
-- https://github.com/haskell-servant/servant/issues/685 is somehow resolved
instance (HasServer api context, MimeUnrender JSON a, FromJSON a, ToSchema a)
  => HasServer (JsonReqBody a :> api) context where
  type ServerT (JsonReqBody a :> api) m = a -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy @api) pc nt . s

  route _ context subserver =
    route (Proxy @api) context
      $ addBodyCheck subserver contentTypeCheck bodyCheck
    where
      contentTypeCheck =
        withRequest $ \request -> do
          fromMaybeM (delayedFailFatal mediaTypeError) $ do
            ct <- lookup Http.hContentType (Wai.requestHeaders request)
            canHandleCTypeH (Proxy @'[ JSON ]) $ LByteString.fromStrict ct
      bodyCheck f =
        withRequest $ \request -> do
          mrqbody <- f <$> liftIO (Wai.lazyRequestBody request)
          onLeft (delayedFailFatal . parseError) mrqbody

      parseError message =
        Error.servantErrResponse
          $ Error.errorResponse @400 @"invalid_request_body"
          $ #message @= Text.pack message
              <: emptyRecord

      mediaTypeError =
        Error.servantErrResponse
          $ Error.errorResponse @415 @"unsupported_media_type" emptyRecord
