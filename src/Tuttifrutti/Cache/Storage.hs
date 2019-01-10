module Tuttifrutti.Cache.Storage where

import           Tuttifrutti.Prelude

import           Data.Range.Range    (Range (..))

-- | Storage implementation. This defines primitive operations, from which
--   all others are derived.
--
--   Parameters:
--
--   [@k@ – key] Each cache item is uniquily identified by the value of this type.
--   [@p@ – priority] Each cache item has a priority that's used for ordering,
--   validity checks and bulk actions (e.g. expiration).
--   [@v@ – value] The type for the content of cache items.
--   [@m@ – monad] The monad type in which the actions would be executed.
data Handle k p v m = Handle
  { -- ^ Alter a value under a specific key.
    --
    --   Takes a function that will be applied to the value (or 'Nothing')
    --   and would return a new value (or 'Nothing') along with the
    --   result that would be returned to the caller.
    --
    --   Allows to implement insert, update, delete and many other useful things (see below).
    alter     :: forall a. (Maybe (p, v) -> (a, Maybe (p, v))) -> k -> m a
    -- ^ Drops elements whose priority falls into a given range.
  , dropRange :: Range p -> m ()
  }

-- | Allows to change the monad in which the storage actions are run.
transHandle :: (forall a. m0 a -> m1 a) -> Handle k p v m0 -> Handle k p v m1
transHandle nat Handle{..} = Handle
  { dropRange = \p -> nat $ dropRange p
  , alter = \f k -> nat $ alter f k
  }

-- | Insert a value.
insert :: Handle k p v m -> (k, p, v) -> m ()
insert h (k, p, v) = alter h (const ((), Just (p, v))) k

-- | Delete a value under given key , returns the deleted value and its priority.
delete :: Functor m => Handle k p v m -> k -> m (Maybe (p, v))
delete h = alter h (,Nothing)

-- | Lookup a value under a key. Takes a function that checks whether
--   the element is expired. If that function returns 'Nothing' the item
--   will be evicted.
lookupValid
  :: Handle k p v m
  -> k
  -> (p -> v -> Maybe a)
  -> m (Maybe a)
lookupValid h k validate =
  alter h
    (maybe (Nothing, Nothing)
      (\pv@(uncurry validate -> a) -> (a, pv <$ a)))
    k

-- | Drop elements whose priority is less than given value.
dropLowerThan :: Handle k p v m -> p -> m ()
dropLowerThan h = dropRange h . UpperBoundRange
