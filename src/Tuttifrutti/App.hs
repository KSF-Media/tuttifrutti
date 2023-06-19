{-# LANGUAGE AllowAmbiguousTypes #-}
module Tuttifrutti.App where

import           Tuttifrutti.Prelude

import qualified Network.Wai            as Wai
import           Servant

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
  :: forall (api :: Type) env.
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

fruttyRioAppContextWith
  :: forall (api :: Type) context env .
     ( HasServer api context
     , Has Http.Handle env
     , Has Log.Handle  env
     , HasContextEntry (context .++ DefaultErrorFormatters)
                       ErrorFormatters
     )
  => Context context
  -> env
  -> ServerT api (RIO env)
  -> Wai.Application
fruttyRioAppContextWith context env server =
  requestLogger env $ \requestId ->
    rioAppContextWith (Proxy @api) (withXRequestId requestId) context env server

-- | Serve an app with given transformations.
rioAppWith
  :: forall (api :: Type) env m. HasServer api '[]
  => Proxy api
  -> (forall a. m a -> RIO env a)
  -> env
  -> ServerT api m
  -> Wai.Application
rioAppWith api f env = rioApp api env . hoistServer api f

-- | Serve an app that lives in RIO monad.
rioApp
  :: forall (api :: Type) env. HasServer api '[]
  => Proxy api
  -> env
  -> ServerT api (RIO env)
  -> Wai.Application
rioApp api env = serve api . hoistServer api (rioHandlerNat env)

rioAppContextWith
  :: forall (api :: Type) context env m. (HasServer api context, ServerContext context)
  => Proxy api
  -> (forall a. m a -> RIO env a)
  -> Context context
  -> env
  -> ServerT api m
  -> Wai.Application
rioAppContextWith api f context env = rioAppContext api context env . hoistServerWithContext api (Proxy @context) f

rioAppContext
  :: forall (api :: Type) context env. (HasServer api context, ServerContext context)
  => Proxy api
  -> Context context
  -> env
  -> ServerT api (RIO env)
  -> Wai.Application
rioAppContext api context env =
  serveWithContext api context . hoistServerWithContext api (Proxy @context) (rioHandlerNat env)

rioHandlerNat :: env -> (forall a. RIO env a -> Handler a)
rioHandlerNat env = Handler . ExceptT . try . runRIO env

handlerRioNat :: Handler a -> RIO env a
handlerRioNat = liftIO . throwLeft . runHandler
