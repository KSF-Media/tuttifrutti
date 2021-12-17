{-# LANGUAGE AllowAmbiguousTypes #-}
module Tuttifrutti.Cache where

import           Tuttifrutti.Prelude

import qualified Data.Has                  as Has
import qualified Tuttifrutti.Cache.Storage as Storage

-- | A cache handle.
newtype Handle id k v = Handle
  { handleStorage :: Storage.Handle k UTCTime v IO }

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
newHandle = pure . Handle

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
  :: forall id k v env m e
   . ( MonadCache env m id k v
     , MonadUnliftIO m
     , Exception e
     )
  => k -- ^ key we are interested in
  -> UTCTime -- ^ everything prior is considered expired and removed
  -> m (Either e (Maybe v))
lookupAfter k threshold = try $ do
  Handle{..} :: Handle id k v <- asks Has.getter
  liftIO $ do
    Storage.dropLowerThan handleStorage threshold
    Storage.lookupValid handleStorage k $ \p v ->
      v <$ guard (p >= threshold)
