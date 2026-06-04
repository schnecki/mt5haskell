module MT5.Communication.PyProc
    ( PyProc (..)
    , pyProc
    , mt5DaemonOwner
    , withMT5Lock
    , registerReconnectAction
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import           Control.Exception       (IOException, catch, throwIO)
import           Data.IORef
import           System.IO
import           System.IO.Error         (isEOFError, isResourceVanishedError)
import           System.IO.Unsafe        (unsafePerformIO)


-- | Connection to the MT5 daemon (socket-based).
--
-- Both 'pyIn' and 'pyOut' point to the same socket handle; kept separate so
-- the 'send' / 'receive' call-sites need no changes.
data PyProc = PyProc
    { pyIn      :: !Handle  -- ^ Read handle (socket)
    , pyOut     :: !Handle  -- ^ Write handle (same socket)
    , pyCleanup :: !(IO ()) -- ^ Close the connection
    }

-- | Active daemon connection; Nothing until 'startMT5' has been called.
pyProc :: IORef (Maybe PyProc)
pyProc = unsafePerformIO $ newIORef Nothing
{-# NOINLINE pyProc #-}

-- | True when this process started the daemon (and is responsible for
-- sending SHUTDOWN and stopping the RPyC server thread).
mt5DaemonOwner :: IORef Bool
mt5DaemonOwner = unsafePerformIO $ newIORef False
{-# NOINLINE mt5DaemonOwner #-}

-- | Global mutex serialising all send/receive exchanges with the MT5 daemon.
-- The Python bridge uses a single shared socket handle; concurrent threads
-- interleave their send/receive sequences and read each other's responses.
-- Every complete request/response cycle must be wrapped with this lock.
pyProcLock :: MVar ()
pyProcLock = unsafePerformIO $ newMVar ()
{-# NOINLINE pyProcLock #-}

-- | Reconnect callback registered by 'MT5.Init.connectToDaemon'.
-- 'withMT5Lock' invokes this when a broken-pipe or EOF error is detected,
-- then retries the action once.  Starts as a no-op until Init registers it.
pyProcReconnectAction :: IORef (IO ())
pyProcReconnectAction = unsafePerformIO $ newIORef (return ())
{-# NOINLINE pyProcReconnectAction #-}

-- | Register the reconnect action (called by 'MT5.Init' after each successful connect).
registerReconnectAction :: IO () -> IO ()
registerReconnectAction = writeIORef pyProcReconnectAction

-- | Run an IO action as an atomic MT5 request/response cycle.
-- On broken-pipe or EOF, reconnects via the registered action and retries once.
withMT5Lock :: IO a -> IO a
withMT5Lock action = withMVar pyProcLock $ \_ ->
  action `catch` \e ->
    if isResourceVanishedError e || isEOFError (e :: IOException)
      then do
        reconnect <- readIORef pyProcReconnectAction
        reconnect
        action
      else throwIO e
