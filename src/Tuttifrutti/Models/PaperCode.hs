{-# LANGUAGE TemplateHaskell #-}
module Tuttifrutti.Models.PaperCode where

import           Tuttifrutti.Prelude

import qualified Data.Text           as Text
import           Data.Unjson
import           Database.Persist.TH (derivePersistField)

data PaperCode
  = HBL
  | ON
  | VN
  | HT
  deriving (Show, Eq, Generic, Read, Data, Ord)
derivePersistField "PaperCode"

instance FromJSON PaperCode
instance ToJSON PaperCode
instance Unjson PaperCode where
  unjsonDef = unjsonAeson

toPaperCode :: Text -> PaperCode
toPaperCode paperCodeText =
  case Text.toUpper paperCodeText of
    "HBL" -> HBL
    "ON"  -> ON
    "ÖN"  -> ON
    "ÖNY" -> ON
    "VN"  -> VN
    "HT"  -> HT
    _     -> HBL
