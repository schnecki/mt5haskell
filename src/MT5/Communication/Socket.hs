-- | Unix domain socket client for the MT5 daemon
module MT5.Communication.Socket
    ( socketPath
    , isSocketAvailable
    , connectSocketHandle
    ) where

import           Control.Exception  (SomeException, try)
import           Network.Socket     (Family (..), SockAddr (..), SocketType (..),
                                     close, connect, defaultProtocol, socket)
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
