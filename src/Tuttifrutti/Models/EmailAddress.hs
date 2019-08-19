module Tuttifrutti.Models.EmailAddress where

import           Tuttifrutti.Prelude

import           Data.Swagger


newtype EmailAddress = EmailAddress { unEmailAddress :: Text }
  deriving ( Show, Eq, Ord
           , Generic, Data, Typeable
           , FromHttpApiData, ToHttpApiData
           , ToJSON, FromJSON
           , IsString
           )

instance ToSchema EmailAddress where
  declareNamedSchema = genericDeclareNamedSchemaNewtype defaultSchemaOptions declareSchema
