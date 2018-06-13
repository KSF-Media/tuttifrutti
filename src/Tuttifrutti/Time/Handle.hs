module Tuttifrutti.Time.Handle
  ( -- * Interface
    Handle(..)
  , handleSystemTime
    -- * Implementations
  , newRealTime
  , newFakeTime
  , newFrozenTime
  ) where

import           Prelude  (Monad, pure, ($))
import           RIO      (IO, MonadIO (..))
import qualified RIO.Time as Time

data Handle = Handle
   { handleCurrentTime :: IO Time.UTCTime }

handleSystemTime :: Handle -> IO Time.UTCTime
handleSystemTime _handle = Time.getCurrentTime

-- | Real time is as simple as it gets.
--
--   currentTime: real system time
--
--   systemTime: real system time
newRealTime :: Monad m => m Handle
newRealTime = pure $ Handle
  { handleCurrentTime = Time.getCurrentTime }

-- | Fake time allows to fake the "current time".
--
--   currentTime: starts at the @initial@ and moves
--                along with real system time
--
--   systemTime: real system time
newFakeTime :: MonadIO m => Time.UTCTime -> m Handle
newFakeTime initial = do
  initializedAt <- liftIO Time.getCurrentTime
  pure Handle
    { handleCurrentTime = do
        now <- Time.getCurrentTime
        let passed = Time.diffUTCTime now initializedAt
        pure $ Time.addUTCTime passed initial

    }

-- | Fake the "current time" by setting it to constant value.
--
--   currentTime: always returns the @frozenAt@
--
--   system time: real system time
newFrozenTime :: Monad m => Time.UTCTime -> m Handle
newFrozenTime frozenAt = pure Handle
  { handleCurrentTime = pure frozenAt }

-- Ideas for more fun handles:
-- - newLeapTime, produces leap seconds every now and then
-- - counterTime, increments the time every time you observe it
