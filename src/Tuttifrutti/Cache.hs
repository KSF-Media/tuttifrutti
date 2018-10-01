{-# LANGUAGE BangPatterns #-}
module Tuttifrutti.Cache where

import           Tuttifrutti.Prelude

import qualified Data.ByteString         as ByteString
import           Data.HashPSQ            (HashPSQ)
import qualified Data.HashPSQ            as HashPSQ


newtype CacheIO = CacheIO { cacheIORef :: IORef Cache }

newCacheIO :: MonadIO m => Cache -> m CacheIO
newCacheIO = fmap CacheIO . newIORef

modifyIO :: MonadIO m => CacheIO -> (Cache -> (Cache, a)) -> m a
modifyIO (CacheIO cacheRef) =
  atomicModifyIORef' cacheRef

modifyIO_ :: MonadIO m => CacheIO -> (Cache -> Cache) -> m ()
modifyIO_ cacheIO f = modifyIO cacheIO $ (,()) . f

insertIO :: MonadIO m => CacheIO -> CacheKey -> UTCTime -> ByteString -> m ()
insertIO (CacheIO cacheRef) k p v =
  atomicModifyIORef' cacheRef $ (,()) . insert k p v

lookupValidIO
  :: MonadIO m
  => CacheIO
  -> CacheKey
  -> (UTCTime -> ByteString -> Maybe a)
  -> m (Maybe a)
lookupValidIO cacheIO key validate = do
  modifyIO cacheIO $ \oldCache ->
    -- we lookup and validate the item under 'cacheKey'
    case lookupValid key validate oldCache of
      -- if that item wasn't valid we get trimmed cache back and save it
      Left newCache -> (newCache, Nothing)
      -- otherwise we get back the found body and the cache stays as-is
      Right body    -> (oldCache, Just body)

-- | Cache-key consists of the important 'Request' fields (operation + params)
--   that are smashed into a single 'ByteString' for the sake of space-efficiency.
--
--   TODO: Try 'SmallByteString'. ByteString is unpinned, and having many small ones
--   can lead to heap fragmentation, 'SmallByteString' might "pack" better inside 'HashPSQ',
--   but I don't want to optimise without a benchmark.
newtype CacheKey = CacheKey { unCacheKey :: ByteString }
  deriving (Ord, Show, Eq, Hashable)

data Cache = Cache
  { -- ^ Amount of memory (in bytes) occupied by the cached bytestrings.
    --   This is only concerned with the actual bytestrings (keys and values).
    --   It doesn't account for the space occupied by the queue structure itself.
    --   For details on how this gets calculated see 'occupiedSpace'.
    cacheSize     :: {-# UNPACK #-} !Int
    -- ^ 'cacheSize' should not exceed this limit
  , cacheCapacity :: {-# UNPACK #-} !Int
    -- ^ The cache queue itself. The 'HashPSQ' is an efficient implementation of a priority queue.
    --   UTCTime is used as a 'priority' which allows us efficiently drop the least recently used items
    --   (see 'trim') and ignore the expired items (see 'lookup').
    --
    -- There is no UNPACK pragma on that field as we want it to be boxed.
    -- I assume that otherwise it'll have to be boxed/unboxed every time we pass
    -- it to lazy HashPSQ functions.
  , cacheQueue    :: !(HashPSQ CacheKey UTCTime ByteString)
  } deriving (Show, Eq)

-- | Calculates the amount of space (in words) that's occupied by a given 'ByteString'.
--   Assumes 64-bit machine with overhead of 9 words (72 bytes) per bytestring.
--   More info here:
--   - <https://www.stackage.org/haddock/lts-11.1/bytestring-0.10.8.2/Data-ByteString-Short.html#g:2>
--   - <https://ro-che.info/articles/2017-01-25-word8-space>
--   - TODO: Post this code on stackoverflow and link to it
occupiedSpace :: ByteString -> Int
occupiedSpace bs = overheadWords * bytesPerWord + contentBytes
  where
    bytesPerWord = 8  -- assuming 64-bit machine
    overheadWords = 9 -- overhead per bytestring (in words)
    contentBytes = -- one byte per element, rounded up to the nearest word
      roundToWord (ByteString.length bs)
    roundToWord a =
      ceiling @Double (fromIntegral a / fromIntegral bytesPerWord) * bytesPerWord

-- | Create a 'Cache' structure with given 'cacheCapacity'.
--   'cacheCapacity' is an amount of bytes to which the actual cache content is limited.
--
--   Suggested value: half of available memory.
empty :: Int -> Cache
empty capacity = Cache
  { cacheSize = 0
  , cacheCapacity = capacity
  , cacheQueue = HashPSQ.empty
  }

-- | Drop the least recently used elements if the 'cacheSize' exceeds the 'cacheCapacity'.
trim :: Cache -> Cache
trim cache@Cache{..}
  -- don't trim if the capacity is not exceeded
  | cacheSize < cacheCapacity = cache
  -- otherwise trim, and recur strictly (in case if more trimming is needed)
  | otherwise = trim $! cache
      { cacheSize = cacheSize - savedSpace
      , cacheQueue = trimmedQueue
      }
  where
    (savedSpace, trimmedQueue) =
      -- calculate the size of minimal element and remove it
      HashPSQ.alterMin ((,Nothing) . maybe 0 elemSize) $! cacheQueue

    elemSize (CacheKey key, _priority, value) =
      occupiedSpace key + occupiedSpace value

-- | Insert a value under a given key with a given priority.
insert :: CacheKey -> UTCTime -> ByteString -> Cache -> Cache
insert key newPriority newValue cache@Cache{..} =
  trim $! cache -- after insertion we strictly trim to prevent overinflation
    { cacheQueue = newQueue
    , cacheSize =
        cacheSize
          + occupiedSpace newValue           -- add the size of inserted value
          - maybe 0 occupiedSpace oldValue   -- subtract the size of evicted value
          + if isNothing oldValue            -- if the key wasn't there before
            then occupiedSpace (unCacheKey key) -- add the size of the key
            else 0
    }
  where
    (fmap snd -> oldValue, newQueue) =
      HashPSQ.insertView key newPriority newValue cacheQueue

-- | Lookup a value under a key. Takes a function that checks whether
--   the element is expired. If that function returns 'Nothing' the item
--   will be evicted and the trimmed cache is returned as 'Left'.
--   If a valid item is found its value is returned as 'Right'.
lookupValid
  :: forall a.
     CacheKey
  -> (UTCTime -> ByteString -> Maybe a)
  -> Cache
  -> Either Cache a
lookupValid key validate cache@Cache{..} =
  case alteredValue of
    -- the key wasn't found in the queue, return the cache as-is
    Nothing        -> Left cache
    -- the key was found and the item was valid, return the value
    Just (Right a) -> Right a
    -- the key was found, but the item wasn't valid, prune it from returned cache
    Just (Left value) ->
      Left cache
        { cacheQueue = alteredQueue
        , cacheSize =
            cacheSize
              - occupiedSpace (unCacheKey key)
              - occupiedSpace value
        }
  where
    -- we use 'alterItem' function to find/remove the item of interest
    -- and return the found/removed value
    (alteredValue, alteredQueue) = HashPSQ.alter alterItem key cacheQueue

    alterItem
      :: Maybe (UTCTime, ByteString)   -- item (if it was found in the queue)
      -> ( Maybe (Either ByteString a) -- validated value
         , Maybe (UTCTime, ByteString) -- altered item (gets saved to the queue)
         )
    alterItem Nothing = (Nothing, Nothing) -- item wasn't there: nothing in, nothing out
    alterItem (Just item@(priority, value)) =
      -- check whether the item is still valid
      case validate priority value of
        -- seems like it's still valid: keep the element, return the decoded value
        Just a  -> (Just (Right a), Just item)
        -- seems like it's not valid: drop the element, return the dropped bytestring
        Nothing -> (Just (Left value), Nothing)

delete :: CacheKey -> Cache -> Cache
delete key cache@Cache{..} =
  case HashPSQ.deleteView key cacheQueue of
    Nothing -> cache
    Just (_p, v, !queue) -> cache
      { cacheQueue = queue
      , cacheSize =
          cacheSize
            - occupiedSpace (unCacheKey key)
            - occupiedSpace v
      }
