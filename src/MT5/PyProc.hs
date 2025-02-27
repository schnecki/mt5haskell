module MT5.PyProc
    ( PyProc (..)
    , pyProc
    ) where

import           Data.IORef
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process

-- | Python process object, fetchable from within IO so we don't need to pass it to every function/hide it with Reader
pyProc :: IORef PyProc
pyProc = unsafePerformIO $ newIORef (error "PyProc object was not set. You need to call startMT5!" :: PyProc)


-- | Python process handles
data PyProc =
  PyProc
    { pyIn         :: !Handle        -- ^ Python std input
    , pyOut        :: !Handle        -- ^ Python std output
    , pyProcHandle :: !ProcessHandle -- ^ Process handle
    }
