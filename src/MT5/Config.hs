{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module MT5.Config
    ( Config (..)
    , rootDir
    , defaultMT5Config
    ) where

import           Control.Concurrent (ThreadId)
import           Control.DeepSeq
import           Data.Default
import           GHC.Generics
import           System.Directory   (getCurrentDirectory)
import           System.IO.Unsafe   (unsafePerformIO)

rootDir :: FilePath
rootDir = unsafePerformIO getCurrentDirectory


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

