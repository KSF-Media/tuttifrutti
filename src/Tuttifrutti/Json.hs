{-# LANGUAGE OverloadedLists #-}
module Tuttifrutti.Json
  ( toJsonRecord
  , nullifyEmptyStrings
  , nullifyEmptyArrays
  , stripStrings
  , stripPrefix
  , uncapitalize
  ) where

import           Tuttifrutti.Prelude

import qualified Data.Aeson          as Json
import qualified Data.Aeson.KeyMap   as KeyMap
import qualified Data.Char           as Char
import           Data.Constraint
import           Data.Extensible
import qualified Data.List           as List
import qualified Data.Text           as Text

-- | Generically convert a record to a json object.
toJsonRecord :: forall xs. Forall (KeyTargetAre KnownSymbol ToJSON) xs => Record xs -> Json.Value
toJsonRecord =
  Json.Object . KeyMap.fromList . flip appEndo [] . hfoldMap getConst
    . hzipWith
        (\(Compose Dict) v -> Const $ Endo
          ((fromString $ symbolVal $ proxyKeyOf v, toJSON $ getField v):))
        (library :: xs :& Compose Dict (KeyTargetAre KnownSymbol ToJSON))

-- | Go over the 'Json.Value' and turn all the @Json.String ""@ into 'Json.Null'.
nullifyEmptyStrings :: Json.Value -> Json.Value
nullifyEmptyStrings (Json.String "") = Json.Null
nullifyEmptyStrings (Json.Array arr) = Json.Array (nullifyEmptyStrings <$> arr)
nullifyEmptyStrings (Json.Object obj) = Json.Object (nullifyEmptyStrings <$> obj)
nullifyEmptyStrings v = v

-- | Go over the 'Json.Value' and apply `Text.strip` to every 'Json.String'.
stripStrings :: Json.Value -> Json.Value
stripStrings (Json.String s)   = Json.String $ Text.strip s
stripStrings (Json.Array arr)  = Json.Array (stripStrings <$> arr)
stripStrings (Json.Object obj) = Json.Object (stripStrings <$> obj)
stripStrings v                 = v

-- | Go over the 'Json.Value' and turn all the @Json.Array []@ into 'Json.Null'.
nullifyEmptyArrays :: Json.Value -> Json.Value
nullifyEmptyArrays (Json.Array [])   = Json.Null
nullifyEmptyArrays (Json.Array arr)  = Json.Array (nullifyEmptyArrays <$> arr)
nullifyEmptyArrays (Json.Object obj) = Json.Object (nullifyEmptyArrays <$> obj)
nullifyEmptyArrays v                 = v

stripPrefix :: String -> Json.Options
stripPrefix prefix =
  Json.defaultOptions
    { Json.fieldLabelModifier
        = uncapitalize . fromMaybe (error ("Did not find prefix " ++ prefix)) . List.stripPrefix prefix
    }

uncapitalize :: String -> String
uncapitalize (head:rest) = Char.toLower head : rest
uncapitalize []          = []
