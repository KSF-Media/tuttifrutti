module Tuttifrutti.Swagger where

import           Tuttifrutti.Prelude

import           Control.Lens
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import           Data.Swagger

-- | Removes given definitions from the swagger spec, replacing all the references
--   with inline occurence.
dereferenceSchemas :: Definitions Schema -> Swagger -> Swagger
dereferenceSchemas defs =
  over definitions (`InsOrd.difference` defs) . inlineAllSchemas defs

-- | A schema with only one possible value.
--
--   Until https://github.com/OAI/OpenAPI-Specification/issues/1313 is
--   resolved, this is shimmed with a single-value enum.
constSchema :: forall a. ToSchema a => Proxy a -> Schema
constSchema a =
  toSchema a & enum_ .~  Just [ toJSON a ]
