module Tuttifrutti.Cache where

import           Tuttifrutti.Prelude

import           Data.Store                         (Store)

import qualified Tuttifrutti.Cache.Storage          as Storage
import qualified Tuttifrutti.Cache.Storage.InMemory as Storage.InMemory

-- | A cache handle.
newtype Cache k v = Cache
  { cacheStorage :: Storage.Handle k UTCTime v IO }

-- | Create a new cache handle.
new
  :: (Hashable k, Ord k, Store k)
  => Store v
  => MonadIO m
  => Int -> m (Cache k v)
new capacity =
  Cache <$> liftIO (Storage.InMemory.newHandle capacity)

-- | Insert a value in a given cache.
insert
  :: MonadIO m
  => Cache k v -> k -> UTCTime -> v -> m ()
insert Cache{..} k p v =
  liftIO $ Storage.insert cacheStorage (k, p, v)

-- | Lookup a value and validate it with provided function.
--   If the function returns 'Nothing' the value is considered
--   expired and gets removed from the cache.
lookupValid
  :: (Store a)
  => MonadIO m
  => Cache k v
  -> k
  -> (UTCTime -> v -> Maybe a)
  -> m (Maybe a)
lookupValid Cache{..} key validate =
  liftIO $ Storage.lookupValid cacheStorage key validate

-- | Lookup a value that must be fresher than the given timestamp.
--   As a side effect all the values that aren't that fresh are removed.
lookupAfter
  :: (Store v)
  => MonadIO m
  => Cache k v
  -> k -- ^ key we are interested in
  -> UTCTime -- ^ everything prior is considered expired and removed
  -> m (Maybe v)
lookupAfter Cache{..} k threshold = liftIO $ do
  Storage.dropLowerThan cacheStorage threshold
  Storage.lookupValid cacheStorage k $ \p v ->
    v <$ guard (p >= threshold)
