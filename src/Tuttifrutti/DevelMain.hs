module Tuttifrutti.DevelMain where

import           Tuttifrutti.Prelude

import qualified Data.Has               as Has
import qualified Rapid

import qualified Tuttifrutti.Log.Handle as Log

-- | A first-class DevelMain module. These fields can be binded to the top level of app's DevelMain module.
data Module env = Module
  { update     :: IO ()
  , kill       :: IO ()
  , withDevEnv :: forall a. (env -> IO a) -> IO a
  }

data Config env = Config
  { createEnv  :: IO env
  , destroyEnv :: env -> IO ()
  , threads    :: [(String, env -> IO ())]
  }

-- | Create a new 'DevelMain' module.
newDevelMain
  :: forall env. (Typeable env, Has Log.Handle env)
  => Word32 -- ^ rapid id
  -> Config env
  -> Module env
newDevelMain rapidId Config{..} = Module{..}
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
