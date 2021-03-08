module Tuttifrutti.Http.Connection where

import           Tuttifrutti.Prelude

import           Network.HTTP.Client.Internal (Connection (..),
                                               ManagerSettings (..))

data ConnectionProxy = ConnectionProxy
  { connectionProxyRead   :: ByteString -> IO ByteString
    -- ^ called whenever somebody reads from connection
  , connectionProxyUnread :: ByteString -> IO ByteString
    -- ^ called whenever somebody returns some read data
  , connectionProxyWrite  :: ByteString -> IO ByteString
    -- ^ called whenever somebody writes to connection
  , connectionProxyClose  :: IO ()
    -- ^ called to close the connection
  }

-- | Pass the given 'Connection' through a given 'ConnectionProxy'
proxyConnection :: ConnectionProxy -> Connection -> Connection
proxyConnection ConnectionProxy{..} Connection{..} =
  Connection
    { connectionRead = connectionProxyRead =<< connectionRead
    , connectionWrite = \bs -> connectionWrite =<< connectionProxyWrite bs
    , connectionUnread = \bs -> connectionUnread =<< connectionProxyUnread bs
    , connectionClose = connectionProxyClose `finally` connectionClose
    }

proxyConnectionManagerSettings :: IO ConnectionProxy -> ManagerSettings -> ManagerSettings
proxyConnectionManagerSettings makeConnectionProxy ManagerSettings{..} =
  ManagerSettings
    { managerRawConnection = do
        makeRawConnection <- managerRawConnection
        connectionProxy <- makeConnectionProxy
        pure $ \addr host port ->
          proxyConnection connectionProxy <$> makeRawConnection addr host port
    , managerTlsConnection = do
        makeTlsConnection <- managerTlsConnection
        connectionProxy <- makeConnectionProxy
        pure $ \addr host port ->
          proxyConnection connectionProxy <$> makeTlsConnection addr host port
    , managerTlsProxyConnection = do
        makeTlsProxyConnection <- managerTlsProxyConnection
        connectionProxy <- makeConnectionProxy
        pure $ \connstr checkConn serverName ha host port ->
          proxyConnection connectionProxy
            <$> makeTlsProxyConnection connstr checkConn serverName ha host port
    , ..
    }
