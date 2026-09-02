{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module MT5.Config
    ( Config (..)
    , Login (..)
    , CommunicationChannel (..)
    , rootDir
    , defaultMT5Config
    , ExecutionEnvironment(..)
    , PythonEnvironment(..)
    , ExecutionMode(..)
    , withLocalMT5Linux
    , withExecutionMode
    , withFileBridge
    , withFileBridgeCustom
    , withPythonBridge
    , withServerTimeZone
    , mt5Config
    , getConfig
    , setConfig
    , modifyConfig
    ) where

import           Control.DeepSeq
import           Data.Default
import           Data.IORef
import           Data.Time.Zones.All (TZLabel)
import           GHC.Generics
import           System.Directory    (getCurrentDirectory)
import           System.IO.Unsafe    (unsafePerformIO)

-- | Communication channel for MT5 interaction
data CommunicationChannel
  = PythonBridge                      -- ^ Use Python bridge (original method)
  | FileBridge                        -- ^ Use file-based bridge (default Wine paths)
  | FileBridgeCustom FilePath FilePath -- ^ Use file-based bridge with custom paths (request, response)
  deriving (Show, Eq, Generic, NFData)

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

data Login = Login
  { account  :: String
  , password :: String
    -- , server   :: Maybe String
    -- , timeout  :: Maybe Int
  } deriving (Show, Eq, NFData, Generic)


-- Configuration and detection functions will be implemented in Init.hs


data Config =
  Config
    { venvDir                     :: FilePath                   -- ^ Directory where virtualenv will be installed
    , winePython                  :: String                     -- ^ Legacy field (maintain compatibility)
    , winePip                     :: String                     -- ^ Legacy field (maintain compatibility)
    , mt5linuxGitRepo             :: String                     -- ^ Git repository URL for mt5linux
    , mt5linuxLocalPath           :: Maybe FilePath             -- ^ User-specified local repository path
    , pythonEnv                   :: Maybe PythonEnvironment    -- ^ Detected Python environment
    , executionEnv                :: Maybe ExecutionEnvironment -- ^ Detected execution environment
    , preferredMode               :: Maybe ExecutionMode        -- ^ User preference override
    , login                       :: Maybe Login                -- ^ Login information. Nothing means default account.
    , communicationChannel        :: CommunicationChannel       -- ^ How to communicate with MT5 (Python or File)
    , positionManagementChannel   :: CommunicationChannel       -- ^ Separate channel for position management operations (close, modify)
    , serverTimeZone              :: Maybe TZLabel              -- ^ IANA zone of the broker trade-server wall-clock (one per MT5 process). 'Nothing' is a fatal misconfiguration: 'MT5.API.initialize' fails fast, because candle timestamps cannot be converted to UTC without it.
    } deriving (Show, Eq, NFData, Generic)


instance Default Config where
  def = Config
    { venvDir                   = rootDir ++ "/.venv"
    , winePython                = ""
    , winePip                   = ""
    , mt5linuxGitRepo           = "https://github.com/schnecki/mt5linux"
    , mt5linuxLocalPath         = Nothing
    , pythonEnv                 = Nothing
    , executionEnv              = Nothing
    , preferredMode             = Nothing
    , login                     = Nothing
    , communicationChannel      = PythonBridge  -- Default to Python (better data completeness)
    , positionManagementChannel = FileBridge    -- Default to FileBridge (reliable position management)
    , serverTimeZone            = Nothing        -- MUST be set by the caller (see 'withServerTimeZone'); unset is fatal at init.
    }

defaultMT5Config :: Config
defaultMT5Config = def

-- | User-specified local mt5linux path
withLocalMT5Linux :: FilePath -> Config -> Config
withLocalMT5Linux path config = config { mt5linuxLocalPath = Just path }

-- | Force specific execution mode
withExecutionMode :: ExecutionMode -> Config -> Config
withExecutionMode mode config = config { preferredMode = Just mode }

-- | Use file-based communication (default Wine paths)
withFileBridge :: Config -> Config
withFileBridge config = config { communicationChannel = FileBridge }

-- | Use file-based communication with custom paths
withFileBridgeCustom :: FilePath -> FilePath -> Config -> Config
withFileBridgeCustom reqPath respPath config =
  config { communicationChannel = FileBridgeCustom reqPath respPath }

-- | Use Python bridge communication
withPythonBridge :: Config -> Config
withPythonBridge config = config { communicationChannel = PythonBridge }

-- | Set the broker trade-server IANA time zone (mandatory for correct candle
--   timestamp conversion).  One MT5 process connects to exactly one trade server,
--   so a single zone applies to every symbol on the connection.
withServerTimeZone :: TZLabel -> Config -> Config
withServerTimeZone lbl config = config { serverTimeZone = Just lbl }

rootDir :: FilePath
{-# NOINLINE rootDir #-}
rootDir = unsafePerformIO getCurrentDirectory

-- | Global configuration reference
--
-- Default configuration uses FileBridge for communication.
-- Can be updated at runtime using setConfig.
mt5Config :: IORef Config
{-# NOINLINE mt5Config #-}
mt5Config = unsafePerformIO (newIORef def)

-- | Get current configuration
getConfig :: IO Config
getConfig = readIORef mt5Config

-- | Set new configuration
setConfig :: Config -> IO ()
setConfig = writeIORef mt5Config

-- | Modify configuration
modifyConfig :: (Config -> Config) -> IO ()
modifyConfig = modifyIORef mt5Config
