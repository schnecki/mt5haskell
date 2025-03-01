{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module MT5.Config
    ( Config (..)
    , rootDir
    , defaultMT5Config
    , setGlobalConfig
    , getGlobalConfig
    ) where

import           Control.Concurrent (ThreadId)
import           Control.DeepSeq
import           Data.Default
import           Data.IORef
import           GHC.Generics
import           System.Directory   (getCurrentDirectory)
import           System.IO.Unsafe   (unsafePerformIO)


-- | Python process object, fetchable from within IO so we don't need to pass it to every function/hide it with Reader
globalConfig :: IORef Config
globalConfig = unsafePerformIO $ newIORef def

setGlobalConfig :: Config -> IO ()
setGlobalConfig = writeIORef globalConfig

getGlobalConfig :: IO Config
getGlobalConfig = readIORef globalConfig


data Config =
  Config
    { venvDir         :: FilePath -- ^ Directory where virtualenv will be installed
    , winePython      :: String
    , winePip         :: String
    , mt5linuxGitRepo :: String
    } deriving (Show, Eq, NFData, Generic)


instance Default Config where
  def = Config (rootDir ++ "/.venv") "" "" "https://github.com/schnecki/mt5linux"

defaultMT5Config :: Config
defaultMT5Config = def

rootDir :: FilePath
rootDir = unsafePerformIO getCurrentDirectory
