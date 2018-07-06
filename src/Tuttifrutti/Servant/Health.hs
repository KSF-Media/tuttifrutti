module Tuttifrutti.Servant.Health where

import RIO
import Servant

type Api =
  "healthz"
    :> Summary "Health endpoint"
    :> Get '[JSON] Text

server :: Applicative m => ServerT Api m
server = ok

ok :: Applicative m => m Text
ok = pure "OK"
