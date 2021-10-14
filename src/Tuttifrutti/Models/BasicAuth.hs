module Tuttifrutti.Models.BasicAuth where

import           Tuttifrutti.Prelude

import qualified Data.ByteString.Base64 as Base64
import           Data.Swagger
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeLatin1)
import           Servant.API            hiding (BasicAuth)
import           Web.HttpApiData

-- Servant's own Basic Authentication implementation relies on using
-- contexts, this is for implementing it more directly.

newtype BasicAuth = BasicAuth { token :: Text }
  deriving (Show, Read, Generic, Data, Typeable, ToJSON, FromJSON)

instance Eq BasicAuth where
  (BasicAuth a) == (BasicAuth b) = Text.takeWhile (/= '=') a == Text.takeWhile (/= '=') b

instance FromHttpApiData BasicAuth where
  parseHeader h     = BasicAuth <$> parseHeaderWithPrefix "Basic " h
  parseQueryParam p = BasicAuth <$> parseQueryParam p

instance ToHttpApiData BasicAuth where
  toHeader     (BasicAuth t) = "Basic " <> encodeUtf8 t
  toQueryParam (BasicAuth t) = t

instance ToSchema BasicAuth where
  declareNamedSchema = genericDeclareNamedSchemaNewtype defaultSchemaOptions declareSchema

instance ToParamSchema BasicAuth

encodeCredentials :: ByteString -> BasicAuth
encodeCredentials = BasicAuth . decodeLatin1 . Base64.encode
