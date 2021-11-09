{-# LANGUAGE AllowAmbiguousTypes #-}
module Tuttifrutti.App where

import           Tuttifrutti.Prelude

import qualified Network.Wai            as Wai

import           Servant                (Handler (..), HasServer, ServerT, hoistServer, runHandler,
                                         serve)

import qualified Tuttifrutti.Http       as Http
import qualified Tuttifrutti.Log.Handle as Log
import           Tuttifrutti.Wai        (requestLogger, withXRequestId)

-- | Serve an API with all possible fruits enabled out-of-box.
--   Can be used as follows:
--
--       app :: Env -> Wai.Application
--       app env = fruttyRioApp @Api env server
--
fruttyRioApp
  :: forall (api :: *) env.
     ( HasServer api '[]
     , Has Http.Handle env
     , Has Log.Handle  env
     )
  => env
  -> ServerT api (RIO env)
  -> Wai.Application
fruttyRioApp env server =
  requestLogger env $ \requestId ->
    rioAppWith (Proxy @api) (withXRequestId requestId) env server

-- | Like fruttyRioApp, but allows passing more transformations.
fruttyRioAppWithTransformations
  :: forall (api :: *) env.
     ( HasServer api '[]
     , Has Http.Handle env
     , Has Log.Handle  env
     )
  => env
  -> (forall a. RIO env a -> RIO env a)
  -> ServerT api (RIO env)
  -> Wai.Application
fruttyRioAppWithTransformations env f server =
  requestLogger env $ \requestId ->
    rioAppWith (Proxy @api) (f . (withXRequestId requestId)) env server

-- | Serve an app with given transformations.
rioAppWith
  :: forall (api :: *) env m. HasServer api '[]
  => Proxy api
  -> (forall a. m a -> RIO env a)
  -> env
  -> ServerT api m
  -> Wai.Application
rioAppWith api f env = rioApp api env . hoistServer api f

-- | Serve an app that lives in RIO monad.
rioApp
  :: forall (api :: *) env. HasServer api '[]
  => Proxy api
  -> env
  -> ServerT api (RIO env)
  -> Wai.Application
rioApp api env = serve api . hoistServer api (rioHandlerNat env)

rioHandlerNat :: env -> (forall a. RIO env a -> Handler a)
rioHandlerNat env = Handler . ExceptT . try . runRIO env

handlerRioNat :: Handler a -> RIO env a
handlerRioNat = liftIO . throwLeft . runHandler
