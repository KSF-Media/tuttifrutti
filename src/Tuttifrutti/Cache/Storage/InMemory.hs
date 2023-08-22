module Tuttifrutti.Cache.Storage.InMemory where

import           Tuttifrutti.Prelude       hiding (lookup)

import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Short     as ShortByteString
import           Data.HashPSQ              (HashPSQ)
import qualified Data.HashPSQ              as HashPSQ
import           Data.List                 (partition)
import           Data.Range                (Range)
import qualified Data.Range                as Range
import           Data.Store                (Store)
import qualified Data.Store                as Store

import qualified Tuttifrutti.Cache.Storage as Storage

-- | Initialize a cache handle, takes amount of bytes to which the
--   cache content will be limited. Suggested value: half of available memory.
newHandle
  :: forall k p v
   . Hashable k
  => (Store k, Store v)
  => (Ord p, Ord k)
  => Int -- ^ maximum capacity (in bytes, assuming 64-bit machine)
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
  { -- | Amount of memory (in bytes) occupied by the bytestrings in the storage.
    --   This is only concerned with the actual bytestrings (keys and values).
    --   It doesn't account for the space occupied by the queue structure itself.
    --   For details on how this gets calculated see 'occupiedSpace'.
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
  , storageQueue    :: !(HashPSQ ShortByteString p ByteString)
  } deriving (Show)

-- | Insert, update, or delete element under a given key.
alter
  :: forall a p k v
   . (Ord p, Ord k)
  => (Store k, Store v)
  => (Maybe (p, v) -> (a, Maybe (p, v)))
  -> k
  -> Storage k p v
  -> (a, Storage k p v)
alter f (ShortByteString.toShort . Store.encode -> key) Storage{..} =
  let ((a, sizeChange), newQueue) = HashPSQ.alter alterItem key storageQueue
  in (a,) $ trim $! Storage
    { storageQueue = newQueue
    , storageSize = storageSize + sizeChange
    , storageCapacity
    }
  where
    alterItem elem0 =
      let
        -- decode the element from the queue, pass it to 'f' and encode the result
        (a, fmap encodeElem -> elem1) = f $ decodeElem =<< elem0
        -- compute the size difference
        sizeDelta =
          case (elem0, elem1) of
            (Nothing, Nothing) -> 0                         -- nothing changed
            (Nothing, Just e1) -> elemSize e1               -- element added
            (Just e0, Just e1) -> elemSize e1 - elemSize e0 -- element updated
            (Just e0, Nothing) -> negate $ elemSize e0      -- element removed
      in ((a, sizeDelta), elem1)

    decodeElem = hush . traverse Store.decode
    encodeElem = fmap Store.encode
    elemSize (_, v) = occupiedSpace (Left key) + occupiedSpace (Right v)

-- | Drop the least recently used elements if the 'cacheSize' exceeds the 'cacheCapacity'.
trim
  :: forall k p v
   . (Ord k, Ord p)
  => (Store k, Store v)
  => Storage k p v
  -> Storage k p v
trim storage@Storage{..}
  -- don't trim if the capacity is not exceeded
  | storageSize < storageCapacity = storage
  -- otherwise trim, and recur strictly (in case if more trimming is needed)
  | otherwise = trim $! Storage
      { storageSize = storageSize - droppedSize
      , storageQueue = selected
      , storageCapacity = storageCapacity
      }
  where
    (droppedSize, selected) =
      -- calculate the size of minimal element and remove it
      HashPSQ.alterMin ((,Nothing) . maybe 0 elementOccupiedSpace) storageQueue

-- | Drop all the elements whose priority is less than @atMost@.
dropLowerThan :: (Ord p) => p -> Storage k p v -> Storage k p v
dropLowerThan atMost Storage{..} = Storage
  { storageQueue = selected
  , storageSize = storageSize - sum (elementOccupiedSpace <$> dropped)
  , storageCapacity
  }
  where
    (dropped, selected) = HashPSQ.atMostView atMost storageQueue

-- | Drops elements whose priority falls into a given range.
--   Works efficently only for 'UpperBoundRange'. For other ranges
--   it will do naive filter and fully reconstructing the structure.
dropRange :: Ord p => Range p -> Storage k p v -> Storage k p v
dropRange (Range.UpperBoundRange (Range.Bound { Range.boundValue = atMost })) storage =
  -- much better than naive filter thanks to 'HashPSQ.atMostView'
  dropLowerThan atMost storage
dropRange range Storage{..} =
  Storage
    { storageQueue = HashPSQ.fromList selected
    , storageSize = storageSize - sum (elementOccupiedSpace <$> dropped)
    , storageCapacity = storageCapacity
    }
  where
    elements = HashPSQ.toList storageQueue
    (dropped, selected) =
      partition (\(_, p, _) -> Range.inRange range p) elements

-- | Calculates the amount of space (in bytes) that's occupied by a given 'ShortByteString' or 'ByteString'.
--   Assumes 64-bit machine.
--   More info here:
--   - <https://www.stackage.org/haddock/lts-11.1/bytestring-0.10.8.2/Data-ByteString-Short.html#g:2>
--   - <https://ro-che.info/articles/2017-01-25-word8-space>
--   - TODO: Post this code on stackoverflow and link to it
occupiedSpace :: Either ShortByteString ByteString -> Int
occupiedSpace bs = overheadWords * bytesPerWord + contentBytes
  where
    bytesPerWord = 8  -- assuming 64-bit machine
    overheadWords =
      case bs of
        Left _short -> 4 -- overhead per short bytestring (in words)
        Right _long -> 9 -- overhead per bytestring (in words)
    contentBytes = -- one byte per element, rounded up to the nearest word
      roundToWord (either ShortByteString.length ByteString.length bs)
    roundToWord a =
      ceiling @Double (fromIntegral a / fromIntegral bytesPerWord) * bytesPerWord

elementOccupiedSpace :: (ShortByteString, p, ByteString) -> Int
elementOccupiedSpace (k, _p, v) =
  occupiedSpace (Left k) + occupiedSpace (Right v)
