{-# LANGUAGE TemplateHaskell #-}
module MT5.Logging
    ( enableMT5Logging
    , disableMT5Logging
    ) where

import           EasyLogger

enableMT5Logging :: LogDestination -> IO ()
enableMT5Logging = $(initLogger)

disableMT5Logging :: IO ()
disableMT5Logging = $(finalizeLogger)


