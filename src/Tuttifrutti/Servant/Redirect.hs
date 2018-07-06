module Tuttifrutti.Servant.Redirect where

import Tuttifrutti.Prelude

import Servant

type Api = Get '[JSON] Text

redirect :: MonadThrow m => ByteString -> m a
redirect url = throwM $ err307 { errHeaders = [("Location", url)] }
