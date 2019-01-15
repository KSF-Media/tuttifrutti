module Tuttifrutti.Pool
  ( module Tuttifrutti.Pool
  , module Data.Pool
  ) where

import           Tuttifrutti.Prelude
import           Tuttifrutti.Time    (NominalDiffTime, nominalMinute)

import           Data.Pool           hiding (createPool, withResource)
import qualified Data.Pool           as Data.Pool

data Config = Config
  { -- ^ amount of stripes (distinct sub-pools) to maintain
    configStripesAmount :: Int
    -- ^ amount of time for which an unused resource is kept open
  , configUnusedTimeout :: NominalDiffTime
    -- ^ maximum number of resources to keep open per stripe
  , configResourcesMax  :: Int
  } deriving (Show, Eq, Ord)

defaultConfig :: Config
defaultConfig = Config
  { configStripesAmount = 1
  , configUnusedTimeout = 10 * nominalMinute
  , configResourcesMax  = 10
  }

-- | Same as 'Data.Pool.createPool' but takes nicer 'Config' type.
createPool
  :: (MonadUnliftIO m, MonadMask m)
  => m a         -- ^ action to create the resource
  -> (a -> m ()) -- ^ action to destroy the resource
  -> Config
  -> m (Pool a)
createPool create destroy Config{..} =
  withRunInIO $ \runInIO -> do
    liftIO $ Data.Pool.createPool
      (runInIO create) (runInIO . destroy)
      configStripesAmount
      configUnusedTimeout
      configResourcesMax

-- | Same as 'Data.Pool.withResource' but uses 'MonadUnliftIO'.
withResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m b
withResource pool action =
  withRunInIO $ \runInIO -> Data.Pool.withResource pool $ runInIO . action

