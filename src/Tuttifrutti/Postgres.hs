module Tuttifrutti.Postgres
  ( module Tuttifrutti.Postgres
  , module Database.PostgreSQL.Simple
  ) where

import           Control.Monad.Catch        (Handler (..))
import           Control.Retry              (RetryPolicy)
import qualified Control.Retry              as Retry
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import qualified Database.PostgreSQL.Simple as PG
import qualified GHC.IO.Exception           as Exception

import qualified Tuttifrutti.Log            as Log
import qualified Tuttifrutti.Log.Handle     as Log
import           Tuttifrutti.Prelude

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  -- exponential backoff starting at 0.1 second
  Retry.exponentialBackoff (round @Double 0.1e6)
    -- with overall timeout of 1 minute
    & Retry.limitRetriesByCumulativeDelay (round @Double 60e6)

connect :: Log.Handle -> ConnectInfo -> Retry.RetryPolicy -> IO Connection
connect logHandle connectInfo retryPolicy =
  Retry.recovering retryPolicy errorHandlers $ \Retry.RetryStatus{..} -> do
    when (rsIterNumber > 0) $ do
      with logHandle $ do
        Log.logInfo "Retrying to connect to database (${iter_number})"
          [ "iter_number" .= rsIterNumber
          , "cumulative_delay" .= rsCumulativeDelay
          , "previous_delay" .= rsPreviousDelay
          ]
    PG.connect connectInfo
  where
    errorHandlers =
      [ \_retryStatus -> Handler $ \Exception.IOError{..} -> do
          with logHandle $ do
            Log.logError "An IOError occurred in ${location} when connecting to postgres."
              [ "type" .= show ioe_type
              , "location" .= ioe_location
              , "description" .= ioe_description
              , "errno" .= fmap show ioe_errno
              , "filename" .= ioe_filename
              , "handle" .= fmap show ioe_handle
              ]
          -- retry only if the error was raised by libpq
          pure $ ioe_location == "libpq"
      ]

disconnect :: Connection -> IO ()
disconnect = PG.close
