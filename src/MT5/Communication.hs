{-# LANGUAGE OverloadedStrings #-}

-- | MT5 Communication module - unified interface for Python and File-based communication
module MT5.Communication
    ( -- * Communication Backends
      -- ** Python Bridge (Legacy)
      module MT5.Communication.Python
      -- ** File-Based Bridge (Recommended)
    , module MT5.Communication.File
    , module MT5.Communication.Types
    ) where

import           MT5.Communication.File
import           MT5.Communication.Python
import           MT5.Communication.Types
