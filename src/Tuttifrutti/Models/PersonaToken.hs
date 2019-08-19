module Tuttifrutti.Models.PersonaToken where

import           Tuttifrutti.Prelude

import           Data.Swagger
import           Database.Persist            (PersistField)
import           Database.Persist.Postgresql (PersistFieldSql)
import           Web.HttpApiData
import           Web.PathPieces              (PathPiece)


newtype PersonaToken = PersonaToken { unPersonaToken :: Text }
  deriving ( Show, Read, Eq, Ord
           , Generic, Data, Typeable
           , ToJSON, FromJSON
           , IsString, PathPiece
           , PersistField, PersistFieldSql
           )

instance FromHttpApiData PersonaToken where
  parseHeader h     = PersonaToken <$> parseHeaderWithPrefix "OAuth " h
  parseQueryParam p = PersonaToken <$> parseQueryParam p

instance ToHttpApiData PersonaToken where
  toHeader     (PersonaToken t) = "OAuth " <> encodeUtf8 t
  toQueryParam (PersonaToken t) = t

instance ToSchema PersonaToken where
  declareNamedSchema = genericDeclareNamedSchemaNewtype defaultSchemaOptions declareSchema

instance ToParamSchema PersonaToken
