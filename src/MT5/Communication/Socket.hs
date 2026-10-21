-- | Unix domain socket client for the MT5 daemon
module MT5.Communication.Socket
    ( socketPath
    , isSocketAvailable
    , connectSocketHandle
    , daemonPeerPid
    ) where

import           Control.Exception  (SomeException, bracket, try)
import           Network.Socket     (Family (..), SockAddr (..), SocketType (..),
                                     close, connect, defaultProtocol,
                                     getPeerCredential, socket)
import           Network.Socket     (socketToHandle)
import           System.IO          (Handle, IOMode (..), hClose)


-- | Fixed path for the MT5 daemon Unix socket.
socketPath :: FilePath
socketPath = "/tmp/mt5haskell.sock"

-- | Connect to the daemon socket and return a 'Handle' for read/write.
connectSocketHandle :: IO Handle
connectSocketHandle = do
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix socketPath)
    socketToHandle sock ReadWriteMode

-- | Return 'True' when the daemon socket exists and accepts a connection.
isSocketAvailable :: IO Bool
isSocketAvailable = do
    result <- try (connectSocketHandle >>= hClose) :: IO (Either SomeException ())
    return $ case result of
        Left  _ -> False
        Right _ -> True

-- | Process id of the daemon listening on 'socketPath', read from the peer
-- credentials of a probe connection; 'Nothing' when no daemon accepts.
daemonPeerPid :: IO (Maybe Int)
daemonPeerPid = do
    result <- try $ bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
        connect sock (SockAddrUnix socketPath)
        (mPid, _, _) <- getPeerCredential sock
        return (fromIntegral <$> mPid)
    return $ case result :: Either SomeException (Maybe Int) of
        Left  _ -> Nothing
        Right r -> r
