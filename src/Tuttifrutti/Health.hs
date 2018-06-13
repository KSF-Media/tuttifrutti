module Tuttifrutti.Health where

import RIO
import Servant

type Api =
  RootApi
    :<|>
  HealthApi

type RootApi = Get '[JSON] Text

type HealthApi =
  "healthz"
    :> Summary "Health endpoint"
    :> Get '[JSON] Text

server :: Applicative m => ServerT Api m
server = ok :<|> ok

ok :: Applicative m => m Text
ok = pure "OK"
