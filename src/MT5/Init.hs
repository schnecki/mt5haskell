{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module MT5.Init
    ( startMT5
    , stopMT5
    ) where

import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Monad
import qualified Data.ByteString    as B
import           Data.IORef
import           Data.List          (sortOn)
import           Data.Maybe         (fromMaybe)
import           Data.Ord           (Down (..))
import qualified Data.Text          as T
import           EasyLogger
import           GHC.IO
import           GHC.IO.Handle
import           System.Directory   (doesDirectoryExist, makeAbsolute)
import           System.Exit
import           System.IO
import           System.IO.Unsafe   (unsafePerformIO)
import           System.Process     hiding (env)
import           Text.Regex

import           MT5.Communication
import           MT5.Config
import           MT5.PyProc

import           Debug.Trace


venvPython :: Config -> IO FilePath
venvPython config = makeAbsolute (venvDir config ++ "/bin/python")


-- | Python process object, fetchable from within IO so we don't need to pass it to every function/hide it with Reader
mt5ServerThread :: IORef (ThreadId, Handle, Handle)
mt5ServerThread = unsafePerformIO $ newIORef (error "Not yet initialized" :: (ThreadId, Handle, Handle))


-- | Create python process
createPythonProcess :: Config -> IO ()
createPythonProcess config
  -- TODO write python file to /tmp/
 = do
  python <- venvPython config
  (Just inp, Just out, _, phandle) <-
    createProcess (proc python ["main.py"]) {cwd = Just ".", std_in = CreatePipe, std_out = CreatePipe}
  writeIORef pyProc $ PyProc inp out phandle

-- | Clones the repo, installs the libraries on linux and windows side and starts the server and python process.
startMT5 :: Config -> IO Config
startMT5 config = do
  cloneMT5Linux
  createVenv
  config' <- installMT5InWine
  startMT5Server config'
  threadDelay (1 * 10 ^ 6) -- give it some time to startup
  createPythonProcess config'
  return config'
  where
    installMT5InWine :: IO (Config)
    installMT5InWine = do
      winPythons <- lines <$> readProcess "locate" ["python.exe"] []
      when (null winPythons) $
        error "Cannot find python.exe (wine python installation). See output of `locate python.exe`"
      let noVenvWinPythons = fromMaybe winPythons $ toMaybe (filter (not . T.isInfixOf "venv" . T.pack) winPythons)
          winPython = head $ sortOn (Down . getPythonVersion) noVenvWinPythons
          winPip = subRegex (mkRegex "/python.exe") winPython "" ++ "/Scripts/pip.exe"
      $(logInfo) $ "Using Windows Python at " ++ winPython
      $(logInfo) $ "Using Windows pip at " ++ winPip
      pipInstall winPip "MetaTrader5"
      pipUpgrade winPip "MetaTrader5"
      pipInstall winPip "-e /tmp/mt5linux/"
      -- pipInstall winPip "matplotlib"
      -- pipInstall winPip "pandas"
      -- pipInstall winPip "-r /tmp/mt5linux/requirements.txt"
      return $ config {winePython = winPython, winePip = winPip}
      where
        toMaybe :: [a] -> Maybe [a]
        toMaybe [] = Nothing
        toMaybe xs = Just xs
        getPythonVersion :: String -> String
        getPythonVersion str = subRegex (mkRegex "^.*/([pP]ython[0-9]+/python.exe)$") str "\\1"
        pipInstall winPip name = pip winPip ("install " ++ name)
        pipUpgrade winPip name = pipInstall winPip ("--upgrade " ++ name)
        pip winPip cmd = do
          res <- spawnCommand ("/usr/bin/wine " ++ winPip ++ " " ++ cmd) >>= waitForProcess
          when (res /= ExitSuccess) $ error $ "ERROR: Could not run pip " ++ cmd ++ " Code: " ++ show res
    -- | Starts the MT5 Server
    startMT5Server :: Config -> IO ()
    startMT5Server config' = do
      when (winePython config' == "") $ error "Need to call installMT5InWine before startMT5Server"
      python <- venvPython config'
      devNullRead <- openFile "/dev/null" ReadMode
      devNullWrite <- openFile "/dev/null" WriteMode
      threadId <-
        forkIO $
        void $
        runProcess
          python
          ["-m", "mt5linux", winePython config']
          Nothing
          Nothing
          (Just devNullRead)
          (Just devNullWrite)
          (Just devNullWrite)
      $(logInfo) $ "Successfully started mt5linux server. ThreadId: " <> show threadId
      writeIORef mt5ServerThread (threadId, devNullRead, devNullWrite)
    -- | Clones repo to /tmp/mt5linux if it doesn't exist
    cloneMT5Linux :: IO ()
    cloneMT5Linux =
      doesDirectoryExist "/tmp/mt5linux" >>= \exists ->
        when (not exists) $ do callProcess "git" ["clone", mt5linuxGitRepo config, "/tmp/mt5linux"]
    -- | Create venv and install libs. Only performs install if virtualenv is not initialized yet. Reinstall by
    -- deleting the folder [venvDir].
    createVenv :: IO ()
    createVenv = do
      venvExists <- doesDirectoryExist (venvDir config)
      when (not venvExists) $ do
        pythons <-
          filter ('-' `notElem`) . filter (T.isInfixOf "python3." . T.pack) . lines <$>
          readProcess "ls" ["/usr/bin/"] ""
        when (length pythons == 0) $
          error "Could not find a compatible version of python (python <=3.11). Looked for /usr/bin/python3*"
        let python = last pythons
        putStrLn $ "Using python version: " ++ python
        callProcess python ["-m", "venv", venvDir config]
        pipInstall "-e /tmp/mt5linux"
      where
        pipInstall name = pip ("install " ++ name)
        pip cmd = do
          res <- spawnCommand (venvDir config ++ "/bin/pip " ++ cmd) >>= waitForProcess
          when (res /= ExitSuccess) $ error $ "ERROR: Could not run pip " ++ cmd ++ " Code: " ++ show res


-- | Stopping the processes
stopMT5 :: IO ()
stopMT5 = do
  send "QUIT" >> receive >>= B.putStr
  threadDelay (1 * 10^6)
  (threadId, devNullRead, devNullWrite) <- readIORef mt5ServerThread
  killThread threadId
  hClose devNullRead
  hClose devNullWrite
  PyProc inp out pHandle <- readIORef pyProc
  cleanupProcess (Just inp, Just out, Nothing, pHandle)
