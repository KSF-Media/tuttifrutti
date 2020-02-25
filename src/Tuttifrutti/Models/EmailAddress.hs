module Tuttifrutti.Models.EmailAddress where

import           Tuttifrutti.Prelude

import qualified Data.Aeson          as JSON
import qualified Data.Swagger        as Swagger
import qualified Data.Text           as Text
import           Data.Unjson
import           Text.Regex.PCRE     (Regex, makeRegex, matchTest)

newtype EmailAddress = EmailAddress { unEmailAddress :: Text }
  deriving ( Show, Eq, Ord
           , Generic, Data, Typeable
           , FromHttpApiData, ToHttpApiData
           , ToJSON, IsString
           )

instance Swagger.ToParamSchema EmailAddress where
  toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy Text)
    & Swagger.pattern ?~ emailRegex

instance Swagger.ToSchema EmailAddress where
  declareNamedSchema = Swagger.genericDeclareNamedSchemaNewtype Swagger.defaultSchemaOptions Swagger.declareSchema

instance PersistField EmailAddress where
  toPersistValue (EmailAddress e) = toPersistValue e
  fromPersistValue e = fmap EmailAddress $ fromPersistValue e

instance PersistFieldSql EmailAddress where
  sqlType _ = SqlString

instance FromJSON EmailAddress where
  parseJSON = JSON.withText "EmailAddress" $ \email ->
    if isEmailAddressValid $ EmailAddress email
    then pure $ EmailAddress email
    else fail $ show EmailAddressInvalid

instance Unjson EmailAddress where
  unjsonDef = unjsonAeson

data EmailAddressErrors = EmailAddressInvalid
  deriving (Show)

isEmailAddressValid :: EmailAddress -> Bool
isEmailAddressValid (EmailAddress email) = matchTest (makeRegex (Text.unpack emailRegex) :: Regex) $ Text.unpack email

-- | From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Validation
emailRegex :: Text
emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
