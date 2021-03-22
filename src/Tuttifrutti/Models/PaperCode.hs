{-# LANGUAGE TemplateHaskell #-}
module Tuttifrutti.Models.PaperCode where

import           Tuttifrutti.Prelude

import           Data.Aeson          (Value (String), withText)
import qualified Data.Text           as Text
import           Data.Unjson         (Unjson (..), unjsonAeson)
import           Database.Persist.TH (derivePersistField)

data PaperCode
  = HBL
  | ON
  | VN
  | HT
  | UnknownPaperCode Text
  deriving (Show, Eq, Generic, Read, Data, Ord)
derivePersistField "PaperCode"

instance FromJSON PaperCode where
  parseJSON = withText "PaperCode" (pure . toPaperCode)
instance ToJSON PaperCode where
  toJSON = String . fromPaperCode
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
    p     -> UnknownPaperCode p

fromPaperCode :: PaperCode -> Text
fromPaperCode paperCode =
  case paperCode of
    UnknownPaperCode p -> p
    _                  -> tshow paperCode
