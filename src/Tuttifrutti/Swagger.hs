module Tuttifrutti.Swagger where

import           Tuttifrutti.Prelude

import           Control.Lens
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.Set                   as Set
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
constSchema :: forall a. (ToSchema a, ToJSON a) => a -> Schema
constSchema a =
  toSchema @a Proxy & enum_ .~  Just [ toJSON a ]

-- | Get a list of undefined references in a swagger spec
undefinedRefs :: Swagger -> [Text]
undefinedRefs swagger = toList $ Set.difference usedRefs defs
  where
    usedRefs = Set.fromList . catMaybes . map getRef . concatMap (toList . view properties) .
      toList $ swagger ^. definitions
    defs = Set.fromList . map fst . InsOrd.toList $ swagger ^. definitions
    getRef (Inline _) = Nothing
    getRef (Ref ref)  = Just $ getReference ref
