{-# LANGUAGE AllowAmbiguousTypes #-}
module Tuttifrutti.Servant where

import           Tuttifrutti.Prelude

import qualified Network.Wai         as Wai

import           Servant             (Handler (..), HasServer, ServerT, hoistServer, runHandler,
                                      serve)

import           Tuttifrutti.Http    (MonadHttp)
import           Tuttifrutti.Log     (MonadLog)
import           Tuttifrutti.Wai     (requestLogger, withXRequestId)

-- | Serve an API with all possible fruits enabled out-of-box.
fruttyRioApp
  :: forall (api :: *) env m.
     ( HasServer api '[]
     , MonadHttp env m
     , MonadLog env m
     )
  => Proxy api
  -> env
  -> ServerT api (RIO env)
  -> Wai.Application
fruttyRioApp api env server =
  requestLogger env $ \requestId ->
    rioAppWith api (withXRequestId requestId) env server

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
