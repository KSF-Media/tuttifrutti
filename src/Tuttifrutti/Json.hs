{-# LANGUAGE OverloadedLists #-}
module Tuttifrutti.Json
  ( toJsonRecord
  , nullifyEmptyStrings
  , nullifyEmptyArrays
  , stripStrings
  , stripPrefix
  ) where

import           Tuttifrutti.Prelude

import           Data.Aeson          (toJSON)
import qualified Data.Aeson          as Json
import qualified Data.Char           as Char
import qualified Data.List as List
import qualified Data.Text           as Text
import           Data.Constraint
import           Data.Extensible
import qualified Data.HashMap.Lazy   as HashMap

-- | Generically convert a record to a json object.
toJsonRecord :: forall xs. Forall (KeyValue KnownSymbol ToJSON) xs => Record xs -> Json.Value
toJsonRecord =
  Json.Object . HashMap.fromList . flip appEndo [] . hfoldMap getConst
    . hzipWith
        (\(Comp Dict) v -> Const $ Endo
          ((fromString $ symbolVal $ proxyAssocKey v, toJSON $ getField v):))
        (library :: Comp Dict (KeyValue KnownSymbol ToJSON) :* xs)

-- | Go over the 'Json.Value' and turn all the @Json.String ""@ into 'Json.Null'.
nullifyEmptyStrings :: Json.Value -> Json.Value
nullifyEmptyStrings (Json.String "") = Json.Null
nullifyEmptyStrings (Json.Array arr) = Json.Array (nullifyEmptyStrings <$> arr)
nullifyEmptyStrings (Json.Object obj) = Json.Object (nullifyEmptyStrings <$> obj)
nullifyEmptyStrings v = v

-- | Go over the 'Json.Value' and apply `Text.strip` to every 'Json.String'.
stripStrings :: Json.Value -> Json.Value
stripStrings (Json.String s) = Json.String $ Text.strip s
stripStrings (Json.Array arr) = Json.Array (stripStrings <$> arr)
stripStrings (Json.Object obj) = Json.Object (stripStrings <$> obj)
stripStrings v = v

-- | Go over the 'Json.Value' and turn all the @Json.Array []@ into 'Json.Null'.
nullifyEmptyArrays :: Json.Value -> Json.Value
nullifyEmptyArrays (Json.Array []) = Json.Null
nullifyEmptyArrays (Json.Array arr) = Json.Array (nullifyEmptyArrays <$> arr)
nullifyEmptyArrays (Json.Object obj) = Json.Object (nullifyEmptyArrays <$> obj)
nullifyEmptyArrays v = v

stripPrefix :: String -> Json.Options
stripPrefix prefix =
  Json.defaultOptions
    { Json.fieldLabelModifier
        = uncapitalize . fromMaybe (error ("Did not find prefix " ++ prefix)) . List.stripPrefix prefix
    }
  where
    uncapitalize :: String -> String
    uncapitalize (head:rest) = Char.toLower head : rest
    uncapitalize []          = []
