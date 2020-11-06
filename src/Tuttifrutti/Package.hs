-- | Some meta utilities to access Haskell packaging data.
module Tuttifrutti.Package where

import           Prelude
import           Tuttifrutti.Prelude

import qualified Data.Aeson          as Json
import qualified Data.Aeson.Lens     as Json
import qualified Data.Yaml           as Yaml

-- | Get extensions that are enabled by default in a file
--   at a given path.
--   FIXME: Support cabal files.
getDefaultExtensions :: FilePath -> IO [Text]
getDefaultExtensions path = do
  packageInfo :: Json.Value <- Yaml.decodeFileThrow path
  let extensions =
        packageInfo
          ^.. Json.key "default-extensions"
            . Json.values
            . Json._String
  pure extensions
