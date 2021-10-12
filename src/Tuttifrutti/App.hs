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
    rioAppWith (Proxy @api) EmptyContext (withXRequestId requestId) env server

fruttyRioAppWithContext
  :: forall (api :: *) (context :: [*]) env.
     ( HasServer api context
     , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
     , Has Http.Handle env
     , Has Log.Handle  env
     )
  => env
  -> Context context
  -> ServerT api (RIO env)
  -> Wai.Application
fruttyRioAppWithContext env context server =
  requestLogger env $ \requestId ->
    rioAppWith (Proxy @api) context (withXRequestId requestId) env server

-- | Serve an app with given transformations.
rioAppWith
  :: forall (api :: *) (context :: [*]) env m.
     ( HasServer api context
     , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
     )
  => Proxy api
  -> Context context
  -> (forall a. m a -> RIO env a)
  -> env
  -> ServerT api m
  -> Wai.Application
rioAppWith api context f env = rioApp api context env . hoistServerWithContext api (Proxy @context) f

-- | Serve an app that lives in RIO monad.
rioApp
  :: forall (api :: *) (context :: [*]) env.
     ( HasServer api context
     , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
     )
  => Proxy api
  -> Context context
  -> env
  -> ServerT api (RIO env)
  -> Wai.Application
rioApp api context env = serveWithContext api context . hoistServerWithContext api (Proxy @context) (rioHandlerNat env)

rioHandlerNat :: env -> (forall a. RIO env a -> Handler a)
rioHandlerNat env = Handler . ExceptT . try . runRIO env

handlerRioNat :: Handler a -> RIO env a
handlerRioNat = liftIO . throwLeft . runHandler
