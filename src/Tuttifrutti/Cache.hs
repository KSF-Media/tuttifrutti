{-# LANGUAGE BangPatterns #-}
module Tuttifrutti.Cache where

import           Tuttifrutti.Prelude

import           Data.Store                         (Store)

import qualified Tuttifrutti.Cache.Storage          as Storage
import qualified Tuttifrutti.Cache.Storage.InMemory as Storage.InMemory

newtype CacheIO k v = CacheIO { cacheIORef :: Storage.Handle k UTCTime v IO }

newCacheIO
  :: (Hashable k, Ord k, Store k)
  => Store v
  => MonadIO m
  => Int -> m (CacheIO k v)
newCacheIO capacity = CacheIO <$> liftIO (Storage.InMemory.newHandle capacity)

insertIO :: MonadIO m => CacheIO k v -> k -> UTCTime -> v -> m ()
insertIO (CacheIO storage) k p v = liftIO $ Storage.insert storage (k, p, v)

lookupValidIO
  :: (Store a)
  => MonadIO m
  => CacheIO k v
  -> k
  -> (UTCTime -> v -> Maybe a)
  -> m (Maybe a)
lookupValidIO (CacheIO storage) key validate =
  liftIO $ Storage.lookupValid storage key validate

lookupExpiringIO
  :: (Store v)
  => MonadIO m
  => CacheIO k v
  -> k -- ^ key we are interested in
  -> UTCTime -- ^ everything prior is considered expired and removed
  -> m (Maybe v)
lookupExpiringIO (CacheIO storage) k threshold = liftIO $ do
  Storage.dropLowerThan storage threshold
  Storage.lookupValid storage k $ \p v -> v <$ guard (p >= threshold)
