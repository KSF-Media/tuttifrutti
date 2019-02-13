{-# LANGUAGE OverloadedLists #-}
module Tuttifrutti.Json
  ( toJsonRecord
  ) where

import           Tuttifrutti.Prelude

import           Data.Aeson          (toJSON)
import qualified Data.Aeson          as Json
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
