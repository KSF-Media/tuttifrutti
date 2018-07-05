module Tuttifrutti.DevelMain where

import           Tuttifrutti.Prelude

import qualified Data.Has               as Has
import qualified Rapid

import qualified Tuttifrutti.Log.Handle as Log

-- | A first-class DevelMain module. These fields can be binded to the top level of app's DevelMain module.
data Handle env = Handle
  { -- ^ Create env that persists across reloads and start
    --   a webserver thread that uses it.
    --
    --   Running update repeatedly would reload the code in
    --   the webserver thread, but won't recreate the 'Env'.
    update     :: IO ()
    -- ^ Destroy the 'Env' and kill the webserver thread.
    --
    --   Use this function when you want to do full restart.
  , kill       :: IO ()
    -- ^ provide given action with the env value
  , withDevEnv :: forall a. (env -> IO a) -> IO a
  }

runRioDev :: Handle env -> RIO env a -> IO a
runRioDev h m = withDevEnv h $ \env -> runRIO env m

data Config env = Config
  { createEnv  :: IO env
  , destroyEnv :: env -> IO ()
  , threads    :: [(String, env -> IO ())]
  }

-- | Create a new 'DevelMain' handle.
newHandle
  :: forall env. (Typeable env, Has Log.Handle env)
  => Word32 -- ^ rapid id
  -> Config env
  -> Handle env
newHandle rapidId Config{..} = Handle{..}
  where
    update :: IO ()
    update = do
      Rapid.rapid rapidId $ \r -> do
        -- create the 'Env' preserving it across the reloads
        envRef :: IORef env <- Rapid.createRef r ("env" :: String) $ do
          newIORef =<< createEnv
        -- run workers on a dedicated threads,
        -- update them with new code on each reload/update cycle
        !env <- readIORef envRef
        for_ threads $ \(name, work) -> do
          Rapid.restart r name $ work env
        Log.waitHandle $ Has.getter env

    kill :: IO ()
    kill = do
      Rapid.rapid rapidId $ \r -> do
        envRef :: IORef env <- Rapid.createRef r ("env" :: String) $ do
          error "Trying to close non-existing Env"
        destroyEnv =<< readIORef envRef
        Rapid.deleteRef r "env"
        for_ threads $ \(name, _work) ->
          Rapid.stop r name ()

    withDevEnv :: forall a. (env -> IO a) -> IO a
    withDevEnv m = do
      Rapid.rapid rapidId $ \r -> do
        envRef :: IORef env <- Rapid.createRef r ("env" :: String) $ do
         newIORef =<< createEnv
        env <- readIORef envRef
        m env `finally` Log.waitHandle (Has.getter env)

closeHandle :: Handle env -> IO ()
closeHandle = kill
