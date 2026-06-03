module MT5.Communication.PyProc
    ( PyProc (..)
    , pyProc
    , mt5DaemonOwner
    ) where

import           Data.IORef
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)


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
