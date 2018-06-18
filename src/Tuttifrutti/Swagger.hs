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
