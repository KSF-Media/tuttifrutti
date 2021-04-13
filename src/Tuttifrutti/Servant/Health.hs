module Tuttifrutti.Servant.Health where

import           RIO
import           Servant

type Api =
  "healthz"
    :> Summary "Health endpoint"
    :> Get '[JSON] Text

server :: Applicative m => ServerT Api m
server = ok

serverWithChecks :: (Monad m, Applicative m) => m () -> ServerT Api m
serverWithChecks checks = ok <*checks

ok :: Applicative m => m Text
ok = pure "OK"
