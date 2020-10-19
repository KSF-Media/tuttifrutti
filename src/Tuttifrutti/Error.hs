{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}
module Tuttifrutti.Error
  ( ErrorResponse()
  , ErrorType(..)
  , servantErrResponse
  , declareErrorResponse
  , Throws
  , throwErrorType
  ) where

import           Tuttifrutti.Prelude

import           Control.Lens              (at)
import qualified Data.Aeson                as Json
import           Data.Extensible           (type (>:), Assoc (..), Forall,
                                            KeyOf, KeyTargetAre,
                                            Membership (..), Record, TargetOf,
                                            emptyRecord, henumerateFor, (<:),
                                            (@=))
import qualified Data.Swagger              as Swagger
import           Data.Swagger.Declare
import qualified Data.Text                 as Text
import           GHC.TypeLits
import qualified Named
import qualified Network.HTTP.Types.Header as Http
import           Servant                   ((:>))
import           Servant.Server
import           Servant.Swagger

import qualified Tuttifrutti.Swagger       as Swagger


-- | Data type for representing a json error response that
--   maps directly to JSON.
--
--   It takes one type argument @tag@, that is the error's codename.
data ErrorResponse tag = ErrorResponse
  { errorResponseHttpCode    :: Int         -- ^ the most relevant http code for that error
  , errorResponseHttpStatus  :: Text        -- ^ developer-readable description
  , errorResponseExtraFields :: Json.Object -- ^ other info about the error
  } deriving (Show, Eq, Typeable)

instance (Typeable tag, KnownSymbol tag) => Exception (ErrorResponse tag)

-- | A way to generate a schema for an error response.
instance
  ( -- relies on the 'ErrorType' class that's defined further
    ErrorType httpCode tag
  , KnownSymbol (HttpStatusCode httpCode)
    -- all extra fields should have their schemas as well
  , Forall (KeyTargetAre KnownSymbol Swagger.ToSchema) (ExtraFields tag)
  )
  => Swagger.ToSchema (ErrorResponse tag)
  where
    declareNamedSchema _ = do
      extraProperties <- declareExtraProperties
      let errSchema = mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & Swagger.properties .~ fromList
                [ ("description", Swagger.Inline $ Swagger.constSchema description)
                ] <> fromList extraProperties
      -- The schema is not named, we want it to be more-or-less anonymous so that we can
      -- "rebundle" them for different endpoints.
      pure $ Swagger.NamedSchema Nothing $ mempty
        & Swagger.type_ ?~ Swagger.SwaggerObject
        & Swagger.properties .~ fromList
            [ ("http_code", Swagger.Inline $ Swagger.constSchema httpCode)
            , ("http_status", Swagger.Inline $ Swagger.constSchema httpStatus )
            -- TODO: the errSchema however could be named and referenced, so that those extra fields
            -- end up in spec definitions, and generated into models. Currently they are anonymous.
            , (tag, Swagger.Inline errSchema)
            ]
      where
        tag = Text.pack $ symbolVal @tag Proxy
        httpCode = natVal @httpCode Proxy
        httpStatus = Text.pack $ symbolVal @(HttpStatusCode httpCode) Proxy
        description = errorDescription @httpCode @tag Proxy
        declareExtraProperties
          :: Declare (Swagger.Definitions Swagger.Schema)
               [(Text, Swagger.Referenced Swagger.Schema)]
        declareExtraProperties =
          henumerateFor
            (Proxy @(KeyTargetAre KnownSymbol Swagger.ToSchema))
            (Proxy @(ExtraFields tag))
            (liftA2 (:) . aProperty)
            (pure [])
          where
            aProperty
              :: forall field key value
               . ( key ~ KeyOf field
                 , value ~ TargetOf field
                 , KnownSymbol key
                 , Swagger.ToSchema value
                 )
              => Membership (ExtraFields tag) field
              -> Declare (Swagger.Definitions Swagger.Schema)
                   (Text, Swagger.Referenced Swagger.Schema)
            aProperty _ =
              (Text.pack $ symbolVal @key Proxy,)
                 <$> Swagger.declareSchemaRef @value Proxy

instance KnownSymbol tag => Json.ToJSON (ErrorResponse tag) where
  toJSON ErrorResponse{..} = Json.Object $ fromList
    [ "http_code" .= errorResponseHttpCode
    , "http_status" .= errorResponseHttpStatus
    , tag .= errorResponseExtraFields
    ]
    where
      tag = Text.pack $ symbolVal @tag Proxy

type family HttpStatusCode (httpCode :: Nat) where
  HttpStatusCode 400 = "Bad request"
  HttpStatusCode 403 = "Forbidden"
  HttpStatusCode 404 = "Not found"
  HttpStatusCode 405 = "Unsupported method"
  HttpStatusCode 409 = "Conflict"
  HttpStatusCode 415 = "Unsupported media type"
  HttpStatusCode 500 = "Internal server error"

-- | A glorious 'ErrorType' class allows us to declare our errors.
class
  ( KnownNat httpCode
  , KnownSymbol tag
  , KnownSymbol (HttpStatusCode httpCode)
  )
  => ErrorType
       (httpCode :: Nat) -- we declare the error's http code
       (tag :: Symbol)   -- and a stringy tag for matching
  | tag -> httpCode      -- tag determines the http code (for each tag there is only one code)
  where
    -- | The error's extra fields can be declared to carry additional information
    type ExtraFields tag :: [Assoc Symbol Type]
    type ExtraFields tag = '[] -- by default there are none

    -- | A devhuman-readable description for an error, by default it's the same as tag
    errorDescription :: proxy tag -> Text
    errorDescription _ = Text.pack $ symbolVal @(HttpStatusCode httpCode) Proxy

    -- | Encode the error in an 'ErrorResponse', the default should usually suffice, but it can
    --   also be defined in per-error fashion
    errorResponse
      :: (Forall (KeyTargetAre KnownSymbol ToJSON) fields, ToJSON (Record fields))
      => Record fields -> ErrorResponse tag
    errorResponse fields = ErrorResponse{..}
      where
        errorResponseHttpCode = fromIntegral $ natVal @httpCode Proxy
        errorResponseHttpStatus = Text.pack $ symbolVal @(HttpStatusCode httpCode) Proxy
        errorResponseDescription = (errorDescription @httpCode @tag Proxy)
        errorResponseExtraFields =
          case toJSON fields of
            Json.Object o -> o & at "description" ?~ Json.String errorResponseDescription
            _             -> error "not an object, impossible"

declareErrorResponse
  :: forall httpCode tag proxy
   . ( ErrorType httpCode tag
     , Swagger.ToSchema (ErrorResponse tag)
     , KnownSymbol (HttpStatusCode httpCode)
     )
  => proxy tag -> Swagger.Swagger -> Swagger.Swagger
declareErrorResponse _ swagger =
  Swagger.setResponseWith mergeResponses httpCode declareResponse swagger
  where
    httpCode = fromIntegral $ natVal @httpCode Proxy
    httpDescription = Text.pack $ symbolVal @(HttpStatusCode httpCode) Proxy
    declareResponse =
      Swagger.declareResponse @(ErrorResponse tag) Proxy
        <&> Swagger.description .~ httpDescription
        <&> Swagger.schema ?~ Swagger.toSchemaRef @(ErrorResponse tag) Proxy
    mergeResponses a b =
      a & Swagger.schema .~ schema
      where
        responseSchema :: Swagger.Response -> Maybe Swagger.Schema
        responseSchema response = do
          referencedSchema <- response ^. Swagger.schema
          case referencedSchema of
            Swagger.Inline s -> pure s
            Swagger.Ref (Swagger.Reference ref) ->
              swagger ^. Swagger.definitions . at ref
        schema = Swagger.Inline <$> responseSchema a <> responseSchema b


servantErrResponse :: forall tag. KnownSymbol tag => ErrorResponse tag -> ServerError
servantErrResponse err@ErrorResponse{..} = ServerError
  { errHTTPCode = errorResponseHttpCode
  , errReasonPhrase = symbolVal @tag Proxy
  , errBody = Json.encode err
  , errHeaders = [ (Http.hContentType, "application/json;charset=utf-8") ]
  }


-- | States that an endpoint might throw the following error.
data Throws (httpCode :: Nat) (tag :: Symbol)

-- | 'HasServer' instance, doesn't really do anything.
instance
  (HasServer api context)
  => HasServer (Throws httpCode tag :> api) context
  where
    type ServerT (Throws httpCode tag :> api) m =
      ServerT api m

    route Proxy context = route (Proxy @api) context

    hoistServerWithContext Proxy =
      hoistServerWithContext (Proxy @api)

-- | 'HasSwagger' instance adds an appropriate error response to the spec.
instance
  ( HasSwagger api
  , ErrorType httpCode tag
  , Swagger.ToSchema (ErrorResponse tag)
  )
  => HasSwagger (Throws code tag :> api)
  where
    toSwagger Proxy =
      toSwagger (Proxy @api)
        & declareErrorResponse (Proxy @tag)

throwErrorType
  :: forall httpCode tag fields m a.
     ( ErrorType httpCode tag
     , ExtraFields tag ~ fields
     , Forall (KeyTargetAre KnownSymbol ToJSON) fields
     , ToJSON (Record fields)
     , MonadThrow m
     , FieldParams fields (m a)
     )
  => ParamsFn fields (m a)
throwErrorType =
  withFieldParams @fields @(m a)
    $ throwM . servantErrResponse . errorResponse @httpCode @tag

class FieldParams (fields :: [Assoc Symbol Type]) a where
  type ParamsFn fields a :: Type
  withFieldParams :: (Record fields -> a) -> ParamsFn fields a

instance FieldParams '[] a where
  type ParamsFn '[] a = a
  withFieldParams f = f emptyRecord

instance
  ( FieldParams ps a
  , p ~ (key ':> value)
  ) => FieldParams ((key ':> value) ': ps) a where
    type ParamsFn ((key ':> value) ': ps) a = key Named.:! value -> ParamsFn ps a
    withFieldParams f (Named.Arg value) =
      withFieldParams (\r -> f ((fromLabel @key @= value) <: r))


instance ErrorType 415 "unsupported_media_type"

instance ErrorType 400 "invalid_request_body" where
  type ExtraFields "invalid_request_body" =
    '[ "message" >: Text ]
  errorDescription _ = "Could not parse the request body."
