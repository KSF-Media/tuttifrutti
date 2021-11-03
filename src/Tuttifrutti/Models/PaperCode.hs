{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module Tuttifrutti.Models.PaperCode where

import           Tuttifrutti.Prelude

import           Data.Aeson                        (Value (String), withText)
import           Data.Swagger                      (SwaggerType (..), ToSchema,
                                                    declareNamedSchema, enum_,
                                                    type_)
import           Data.Swagger.Internal.ParamSchema (ToParamSchema,
                                                    toParamSchema)
import qualified Data.Text                         as Text
import           Data.Text.Encoding                (decodeUtf8)
import           Data.Unjson                       (Unjson (..), unjsonAeson)
import           Database.Persist.Types            (fromPersistValueText)
import           Web.HttpApiData                   (FromHttpApiData (..))


data PaperCode
  = HBL
  | ON
  | VN
  | HT
  | JUNIOR
  | FORUM
  | LS
  | UnknownPaperCode Text
  deriving (Show, Eq, Generic, Read, Data, Ord, Hashable)

instance FromJSON PaperCode where
  parseJSON = withText "PaperCode" (pure . toPaperCode)
instance ToJSON PaperCode where
  toJSON = String . fromPaperCode
instance Unjson PaperCode where
  unjsonDef = unjsonAeson

instance PersistFieldSql PaperCode where
  sqlType _ = SqlString

instance PersistField PaperCode where
  toPersistValue = PersistText . fromPaperCode
  fromPersistValue =
    \case
      PersistText paperCodeText -> Right $ toPaperCode paperCodeText
      PersistLiteral paperCode -> Right $ toPaperCode $ decodeUtf8 paperCode
      PersistLiteralEscaped paperCode -> Right $ toPaperCode $ decodeUtf8 paperCode
      persistVal ->
        Left $
          "PaperCode decoding error! Expected Text from the database, but got something else: " <>
          -- Some other database types are correctly decoded into `Text`, however
          -- better to explicitly expect it to be `PersistText` always
          (either id id $ fromPersistValueText persistVal)

instance ToSchema PaperCode where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToParamSchema PaperCode where
  toParamSchema _ = mempty
     & type_ ?~ SwaggerString
     & enum_ ?~ (map toJSON [ HBL, ON, VN, HT, JUNIOR, FORUM, LS ])

instance FromHttpApiData PaperCode where
  parseQueryParam = Right . toPaperCode

toPaperCode :: Text -> PaperCode
toPaperCode paperCodeText =
  case Text.toUpper paperCodeText of
    "HBL"    -> HBL
    "ON"     -> ON
    "ÖN"     -> ON
    "ÖNY"    -> ON
    "VN"     -> VN
    "HT"     -> HT
    "JUNIOR" -> JUNIOR
    "FORUM"  -> FORUM
    "LS"     -> LS
    p        -> UnknownPaperCode p

fromPaperCode :: PaperCode -> Text
fromPaperCode paperCode =
  case paperCode of
    UnknownPaperCode p -> p
    _                  -> tshow paperCode
