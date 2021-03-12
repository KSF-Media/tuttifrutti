{-# LANGUAGE TemplateHaskell #-}
module Tuttifrutti.Models.Paper where

import           Tuttifrutti.Prelude

import qualified Data.Text           as Text
import           Database.Persist.TH (derivePersistField)

data Paper
  = HBL
  | ON
  | VN
  | HT
  deriving (Show, Eq, Generic, Read, Data, Ord)
derivePersistField "Paper"

instance FromJSON Paper
instance ToJSON Paper

toPaper :: Text -> Paper
toPaper paperText =
  case Text.toUpper paperText of
    "HBL" -> HBL
    "ON"  -> ON
    "ÖN"  -> ON
    "ÖNY" -> ON
    "VN"  -> VN
    "HT"  -> HT
    _     -> HBL
