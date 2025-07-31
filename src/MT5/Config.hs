{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
module MT5.Config
    ( Config (..)
    , rootDir
    , defaultMT5Config
    , setGlobalConfig
    , getGlobalConfig
    , ExecutionEnvironment(..)
    , PythonEnvironment(..)
    , ExecutionMode(..)
    , withLocalMT5Linux
    , withExecutionMode
    ) where

import           Control.DeepSeq
import           Data.Default
import           Data.IORef
import           GHC.Generics
import           System.Directory   (getCurrentDirectory)
import           System.IO.Unsafe   (unsafePerformIO)

-- | Execution environment detection
data ExecutionEnvironment = 
    WineEnvironment     -- ^ Linux with Wine-accessible Windows Python
  | WSLEnvironment      -- ^ WSL with direct Windows Python access  
  | NativeLinux         -- ^ Pure Linux (fallback, may not support MT5)
  deriving (Show, Eq, Generic, NFData)

-- | Python environment configuration
data PythonEnvironment = PythonEnvironment
  { pythonExecutable :: FilePath
  , pipExecutable    :: FilePath  
  , executionMode    :: ExecutionMode
  } deriving (Show, Eq, Generic, NFData)

-- | Command execution mode
data ExecutionMode =
    WineExecution       -- ^ Prefix commands with "wine"
  | DirectExecution     -- ^ Execute Windows binaries directly (WSL)
  deriving (Show, Eq, Generic, NFData)

-- Configuration and detection functions will be implemented in Init.hs


-- | Python process object, fetchable from within IO so we don't need to pass it to every function/hide it with Reader
globalConfig :: IORef Config
globalConfig = unsafePerformIO $ newIORef def

setGlobalConfig :: Config -> IO ()
setGlobalConfig = writeIORef globalConfig

getGlobalConfig :: IO Config
getGlobalConfig = readIORef globalConfig


data Config =
  Config
    { venvDir           :: FilePath                     -- ^ Directory where virtualenv will be installed
    , winePython        :: String                       -- ^ Legacy field (maintain compatibility)
    , winePip           :: String                       -- ^ Legacy field (maintain compatibility)
    , mt5linuxGitRepo   :: String                      -- ^ Git repository URL for mt5linux
    , mt5linuxLocalPath :: Maybe FilePath              -- ^ User-specified local repository path
    , pythonEnv         :: Maybe PythonEnvironment     -- ^ Detected Python environment
    , executionEnv      :: Maybe ExecutionEnvironment  -- ^ Detected execution environment
    , preferredMode     :: Maybe ExecutionMode         -- ^ User preference override
    } deriving (Show, Eq, NFData, Generic)


instance Default Config where
  def = Config 
    { venvDir = rootDir ++ "/.venv"
    , winePython = ""
    , winePip = ""
    , mt5linuxGitRepo = "https://github.com/schnecki/mt5linux"
    , mt5linuxLocalPath = Nothing
    , pythonEnv = Nothing
    , executionEnv = Nothing
    , preferredMode = Nothing
    }

defaultMT5Config :: Config
defaultMT5Config = def

-- | User-specified local mt5linux path
withLocalMT5Linux :: FilePath -> Config -> Config
withLocalMT5Linux path config = config { mt5linuxLocalPath = Just path }

-- | Force specific execution mode
withExecutionMode :: ExecutionMode -> Config -> Config
withExecutionMode mode config = config { preferredMode = Just mode }

rootDir :: FilePath
rootDir = unsafePerformIO getCurrentDirectory
