{-# LANGUAGE AllowAmbiguousTypes #-}
module Tuttifrutti.Cache where

import           Tuttifrutti.Prelude

import qualified Data.Has                  as Has
import qualified Tuttifrutti.Cache.Storage as Storage

import qualified Tuttifrutti.Cache.Nursery          as Nursery

-- | A cache handle.
data Handle id k v = Handle
  { handleStorage :: Storage.Handle k UTCTime v IO
  , handleNursery :: Nursery.Handle k UTCTime v
  }

type MonadCache env m id k v =
  ( MonadReader env m
  , Has (Handle id k v) env
  , MonadIO m
  )

-- | Create a new cache handle.
newHandle
  :: (Hashable k, Ord k)
  => MonadIO m
  => Storage.Handle k UTCTime v IO
  -> m (Handle id k v)
newHandle handleStorage = do
  handleNursery <- atomically Nursery.newHandle
  pure Handle
    { handleNursery
    , handleStorage = Nursery.nursedIOStorage handleNursery handleStorage
    }

waitHandle :: MonadIO m => Handle id k v -> m ()
waitHandle Handle{..} =
  Nursery.waitHandle handleNursery

insertAsync
  :: forall id k v env m. (MonadCache env m id k v, Hashable k, Ord k)
  => k -> UTCTime -> Async (UTCTime, v) -> m ()
insertAsync k now asyncPV = do
  Handle{..} :: Handle id k v <- asks Has.getter
  atomically $ Nursery.insertAsync handleNursery k now asyncPV

-- | Insert a value in a given cache.
insert :: forall id k v env m. MonadCache env m id k v => k -> UTCTime -> v -> m ()
insert k p v = do
  Handle{..} :: Handle id k v <- asks Has.getter
  liftIO $ Storage.insert handleStorage (k, p, v)

delete :: forall id k v env m. MonadCache env m id k v => k -> m ()
delete k = do
  Handle{..} :: Handle id k v <- asks Has.getter
  void $ liftIO $ Storage.delete handleStorage k

lookup :: forall id k v env m. (MonadCache env m id k v) => k -> m (Maybe v)
lookup k = do
  lookupValid @id k (const Just)

lookupAsync
  :: forall id k v env m
   . (MonadCache env m id k v, Hashable k, Ord k)
  => k -> m (Maybe (Async (UTCTime, v)))
lookupAsync k = do
  Handle{..} :: Handle id k v <- asks Has.getter
  atomically $ Nursery.lookupAsync handleNursery k

-- | Lookup a value and validate it with provided function.
--   If the function returns 'Nothing' the value is considered
--   expired and gets removed from the cache.
lookupValid
  :: forall id k v a env m
   . MonadCache env m id k v
  => k
  -> (UTCTime -> v -> Maybe a)
  -> m (Maybe a)
lookupValid key validate = do
  Handle{..} :: Handle id k v <- asks Has.getter
  liftIO $ Storage.lookupValid handleStorage key validate

-- | Lookup a value that must be fresher than the given timestamp.
--   As a side effect all the values that aren't that fresh are removed.
lookupAfter
  :: forall id k v env m
   . MonadCache env m id k v
  => k -- ^ key we are interested in
  -> UTCTime -- ^ everything prior is considered expired and removed
  -> m (Maybe v)
lookupAfter k threshold = do
  Handle{..} :: Handle id k v <- asks Has.getter
  liftIO $ do
    Storage.dropLowerThan handleStorage threshold
    Storage.lookupValid handleStorage k $ \p v ->
      v <$ guard (p >= threshold)
