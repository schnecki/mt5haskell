module MT5.Communication.PyProc
    ( PyProc (..)
    , pyProc
    ) where

import           Data.IORef
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process

-- | Python process object, fetchable from within IO so we don't need to pass it to every function/hide it with Reader
-- | Initialized to Nothing; set by startMT5
pyProc :: IORef (Maybe PyProc)
pyProc = unsafePerformIO $ newIORef Nothing
{-# NOINLINE pyProc #-}


-- | Python process handles
data PyProc =
  PyProc
    { pyIn         :: !Handle        -- ^ Python std input
    , pyOut        :: !Handle        -- ^ Python std output
    , pyProcHandle :: !ProcessHandle -- ^ Process handle
    , pyServerPath :: !FilePath      -- ^ File path to code
    }
