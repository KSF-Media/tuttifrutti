{-# LANGUAGE BlockArguments #-}
module Tuttifrutti.Cache.Nursery where

import           Tuttifrutti.Prelude

import           Data.HashPSQ              (HashPSQ)
import qualified Data.HashPSQ              as HashPSQ
import qualified Data.Time                 as Time
import qualified Tuttifrutti.Time          as Time

import qualified Tuttifrutti.Cache.Storage as Storage

data Handle k p v = Handle
  { nurseryQueue :: TVar (HashPSQ k UTCTime (Async (p, v))) }

nursedIOStorage
  :: (Hashable k, Ord k, Ord p)
  => MonadIO m
  => Handle k p v
  -> Storage.Handle k p v m
  -> Storage.Handle k p v m
nursedIOStorage = nursedStorage atomically

nursedSTMStorage
  :: (Hashable k, Ord k, Ord p)
  => Handle k p v
  -> Storage.Handle k p v STM
  -> Storage.Handle k p v STM
nursedSTMStorage = nursedStorage id

nursedStorage
  :: (Hashable k, Ord k, Ord p)
  => Monad m
  => (forall a. STM a -> m a)
  -> Handle k p v
  -> Storage.Handle k p v m
  -> Storage.Handle k p v m
nursedStorage liftSTM Handle{..} storage = Storage.Handle
  { alter = \f k -> do
      -- wait/get the the value from nursery (if any)
      nursed <- liftSTM $ traverse waitSTM =<< lookupAsync Handle{..} k
      a <- Storage.alter storage
        (\stored ->
           f $ case (nursed, stored) of
             (Nothing, Nothing) -> Nothing
             (Just pv, Nothing) -> Just pv
             (Nothing, Just pv) -> Just pv
             (Just (nursedP, nursedV), Just (storedP, storedV))
               | nursedP > storedP -> Just (nursedP, nursedV)
               | otherwise -> Just (storedP, storedV)
        )
        k
      liftSTM $ modifyTVar nurseryQueue $ HashPSQ.delete k
      pure a
  , dropRange = Storage.dropRange storage
  }

newHandle :: STM (Handle k p v)
newHandle = do
  nurseryQueue <- newTVar HashPSQ.empty
  pure Handle{..}

insertAsync
  :: (Hashable k, Ord k)
  => Handle k p v -> k -> UTCTime -> Async (p, v) -> STM ()
insertAsync Handle{..} k now v = do
  modifyTVar nurseryQueue $ HashPSQ.insert k now v . trim
  where
    -- remove entries that were sitting there since before the threshold
    trim = snd . HashPSQ.atMostView threshold
    threshold = Time.addUTCTime (negate 10 * Time.nominalMinute) now

lookupAsync
  :: (Hashable k, Ord k)
  => Handle k p v -> k -> STM (Maybe (Async (p, v)))
lookupAsync Handle{..} k =
  fmap snd . HashPSQ.lookup k <$> readTVar nurseryQueue

waitHandle :: MonadIO m => Handle k p v -> m ()
waitHandle Handle{..} =
  mapM_ wait =<< atomically do readTVar nurseryQueue
