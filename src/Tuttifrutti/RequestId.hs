{-# LANGUAGE PatternSynonyms #-}
module Tuttifrutti.RequestId where

import           Tuttifrutti.Prelude

import qualified Data.Aeson.Types          as Json
import qualified Data.CaseInsensitive      as CI
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import qualified Data.UUID                 as UUID
import qualified Data.UUID.V4              as UUID.V4
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai               as Wai

import qualified Tuttifrutti.Log           as Log
import qualified Tuttifrutti.Log.Handle    as Log

newtype RequestId = RequestId Text
  deriving (Show)
  deriving newtype (ToJSON)

random :: MonadIO m => m RequestId
random =
  RequestId . UUID.toText <$> liftIO UUID.V4.nextRandom


pattern XRequestID :: (IsString a, Eq a) => a
pattern XRequestID = "X-Request-ID"

requestIdHeader :: Lens' Http.RequestHeaders (Maybe RequestId)
requestIdHeader =
  lens
    (fmap (RequestId . decodeUtf8) . lookup XRequestID)
    (\headers -> \case
        Nothing -> filter ((/= XRequestID). fst) headers
        Just (RequestId requestId) ->
          map
            (\(k,v) ->
               if k == XRequestID
               then (k, encodeUtf8 requestId)
               else (k,v))
            headers)
