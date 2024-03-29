module Tuttifrutti.Cache.Storage where

import           Tuttifrutti.Prelude hiding (set)

import           Data.Range          (Bound (..), BoundType (..), Range (..))

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
  { -- | Alter a value under a specific key.
    --
    --   Takes a function that will be applied to the value (or 'Nothing')
    --   and would return a new value (or 'Nothing') along with the
    --   result that would be returned to the caller.
    --
    --   Allows to implement insert, update, delete and many other useful things (see below).
    alter     :: forall a. (Maybe (p, v) -> (a, Maybe (p, v))) -> k -> m a
    -- | Inserts (upserts) or deletes value without looking at the old value.
    --
    --   This may be cheaper operation on the backend than using the
    --   full alter.  If omitted the functions in this module will
    --   just use alter.
  , set       :: Maybe (Maybe (p, v) -> k -> m ())
    -- | Drops elements whose priority falls into a given range.
  , dropRange :: Range p -> m ()
  }

-- | Allows to change the monad in which the storage actions are run.
transHandle :: (forall a. m0 a -> m1 a) -> Handle k p v m0 -> Handle k p v m1
transHandle nat Handle{..} = Handle
  { dropRange = \p -> nat $ dropRange p
  , set = fmap (\f s k -> nat $ f s k) set
  , alter = \f k -> nat $ alter f k
  }

-- | Insert a value.
insert :: Handle k p v m -> (k, p, v) -> m ()
insert h (k, p, v) = set h & maybe
  (alter h (const ((), Just (p, v))) k)
  (\f -> f (Just (p, v)) k)

-- | Delete a value under given key , returns the deleted value and its priority.
delete :: Functor m => Handle k p v m -> k -> m (Maybe (p, v))
delete h = alter h (,Nothing)

-- | Delete a value under given key
delete_ :: Functor m => Handle k p v m -> k -> m ()
delete_ h k = set h & maybe
  (void $ delete h k)
  (\f -> f Nothing k)

-- | Lookup a value under a key with no validations.
lookup :: Handle k p v m -> k -> m (Maybe v)
lookup h k = lookupValid h k (const Just)

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
dropLowerThan h p =  dropRange h $ UpperBoundRange (Bound { boundValue = p, boundType = Inclusive })
