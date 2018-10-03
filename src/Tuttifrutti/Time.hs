module Tuttifrutti.Time
  ( MonadTime
  , currentTime
  , applicationTime
  , systemTime
  , currentDay
  , isToday
  , localFakeTime
  , localFrozenTime
  , nominalHour
  , nominalMinute
  , nominalSecond
  ) where

import           Tuttifrutti.Prelude

import qualified Data.Has                as Has
import qualified RIO.Time                as Time

import           Tuttifrutti.Time.Handle

-- | The constraint that enables usage of the interface provided by this module.
type MonadTime env m =
  ( Has Handle env
  , MonadReader env m
  , MonadIO m
  )

-- | Get the "current time". Should be used most of the time for
--   all business-level purposes and app's logic.
--
--   Can and will be faked via 'newFakeTime'.
currentTime :: MonadTime env m => m Time.UTCTime
currentTime = liftIO =<< asks (handleCurrentTime . Has.getter)

-- | A more accurate name for 'currentTime'. Yet 'currentTime' should be used
--   in bussiness-logic to avoid complicating it with app/system-time distinction.
--
--   Nicer to use when we branch between app/system time and want to make that clear.
applicationTime :: MonadTime env m => m Time.UTCTime
applicationTime = currentTime

-- | Shortcut for getting a day from the 'currentTime'.
currentDay :: MonadTime env m => m Time.Day
currentDay = Time.utctDay <$> currentTime

-- | Get the "system time". Should be used in exceptional cases
--   when the real system time is needed. E.g. logging,
--   and other non-business non-application things.
systemTime :: MonadTime env m => m Time.UTCTime
systemTime = liftIO =<< asks (handleSystemTime . Has.getter)

-- | Takes zoned time and tells whether it's still that same day in that timezone.
isToday :: MonadTime env m => Time.ZonedTime -> m Bool
isToday (Time.ZonedTime localTime zone)= do
  currentLocalTime <- Time.utcToLocalTime zone <$> currentTime
  pure $ Time.localDay currentLocalTime == Time.localDay localTime

-- | Run the given action with fake time that starts at given moment. Uses 'newFakeTime'.
localFakeTime :: MonadTime env m => Time.UTCTime -> m a -> m a
localFakeTime initial m = do
  fakeTime <- newFakeTime initial
  local (Has.modifier (const fakeTime)) m

-- | Run the given action with fake time that stays at a given moment. Uses 'newFrozenTime'.
localFrozenTime :: MonadTime env m => Time.UTCTime -> m a -> m a
localFrozenTime frozenAt m = do
  fakeTime <- newFrozenTime frozenAt
  local (Has.modifier (const fakeTime)) m

nominalHour :: Time.NominalDiffTime
nominalHour = nominalMinute * 60

nominalMinute :: Time.NominalDiffTime
nominalMinute = 60

nominalSecond :: Time.NominalDiffTime
nominalSecond = 1
