module Tuttifrutti.Cache.Storage.InMemoryRef where

import           Tuttifrutti.Prelude       hiding (lookup)

import           Data.HashPSQ              (HashPSQ)
import qualified Data.HashPSQ              as HashPSQ
import           Data.List                 (partition)
import           Data.Range                (Range)
import qualified Data.Range                as Range

import qualified Tuttifrutti.Cache.Storage as Storage

-- | Initialize a cache handle, takes amount of entries to which the
--   cache content will be limited.
newHandle
  :: forall k p v
   . Hashable k
  => (Ord p, Ord k)
  => Int -- ^ maximum capacity (number of entries)
  -> STM (Storage.Handle k p v STM)
newHandle capacity = do
  var <- newTVar $ Storage
    { storageQueue = HashPSQ.empty
    , storageSize = 0
    , storageCapacity = capacity
    }
  pure $ Storage.Handle
    { Storage.alter = \f k ->
        stateTVar var (alter f k)
    , Storage.set = Nothing
    , Storage.dropRange = \p ->
        stateTVar var $ ((),) . dropRange p
    }

data Storage k p v = Storage
  { -- | Amount of entries that are kept in the cache at the moment.
    storageSize     :: {-# UNPACK #-} !Int
    -- | 'storageSize' should not exceed this limit
  , storageCapacity :: {-# UNPACK #-} !Int
    -- | The cache queue itself. The 'HashPSQ' is an efficient implementation of a priority queue.
    --   UTCTime is used as a 'priority' which allows us efficiently drop the least recently used items
    --   (see 'trim') and ignore the expired items (see 'lookup').
    --
    -- There is no UNPACK pragma on that field as we want it to be boxed.
    -- I assume that otherwise it'll have to be boxed/unboxed every time we pass
    -- it to lazy HashPSQ functions.
  , storageQueue    :: !(HashPSQ k p v)
  } deriving (Show)

-- | Insert, update, or delete element under a given key.
alter
  :: forall a p k v
   . (Ord p, Ord k, Hashable k)
  => (Maybe (p, v) -> (a, Maybe (p, v)))
  -> k
  -> Storage k p v
  -> (a, Storage k p v)
alter f key Storage{..} =
  let ((a, sizeChange), newQueue) = HashPSQ.alter alterItem key storageQueue
  in (a,) $ trim $! Storage
    { storageQueue = newQueue
    , storageSize = storageSize + sizeChange
    , storageCapacity
    }
  where
    alterItem elem0 =
      let
        -- pass the element from the queue to 'f'
        (a, elem1) = f elem0
        -- compute the size difference
        sizeDelta =
          case (elem0, elem1) of
            (Nothing, Nothing) -> 0  -- nothing changed
            (Nothing, Just _)  -> 1  -- element added
            (Just _, Just _)   -> 0  -- element updated
            (Just _, Nothing)  -> -1 -- element removed
      in ((a, sizeDelta), elem1)

-- | Drop the least recently used elements if the 'cacheSize' exceeds the 'cacheCapacity'.
trim
  :: forall k p v
   . (Ord k, Ord p, Hashable k)
  => Storage k p v
  -> Storage k p v
trim storage@Storage{..}
  -- don't trim if the capacity is not exceeded
  | storageSize < storageCapacity = storage
  -- otherwise trim, and recur strictly (in case if more trimming is needed)
  | otherwise = trim $! Storage
      { storageSize = storageSize - 1
      , storageQueue = HashPSQ.deleteMin storageQueue
      , storageCapacity = storageCapacity
      }

-- | Drop all the elements whose priority is less than @atMost@.
dropLowerThan :: (Ord p, Hashable k, Ord k) => p -> Storage k p v -> Storage k p v
dropLowerThan atMost Storage{..} = Storage
  { storageQueue = selected
  , storageSize = storageSize - length dropped
  , storageCapacity
  }
  where
    (dropped, selected) = HashPSQ.atMostView atMost storageQueue

-- | Drops elements whose priority falls into a given range.
--   Works efficently only for 'UpperBoundRange'. For other ranges
--   it will do naive filter and fully reconstructing the structure.
dropRange :: (Hashable k, Ord k, Ord p) => Range p -> Storage k p v -> Storage k p v
dropRange (Range.UpperBoundRange (Range.Bound { Range.boundValue = atMost })) storage =
  -- much better than naive filter thanks to 'HashPSQ.atMostView'
  dropLowerThan atMost storage
dropRange range Storage{..} =
  Storage
    { storageQueue = HashPSQ.fromList selected
    , storageSize = storageSize - length dropped
    , storageCapacity = storageCapacity
    }
  where
    elements = HashPSQ.toList storageQueue
    (dropped, selected) =
      partition (\(_, p, _) -> Range.inRange range p) elements
