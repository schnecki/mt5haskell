{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module MT5.Init
    ( startMT5
    , stopMT5
    ) where

import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Exception  (catch, SomeException, throwIO)
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
import           System.Directory   (doesDirectoryExist, makeAbsolute, removeDirectoryRecursive)
import           System.Exit
import           System.IO
import           System.IO.Unsafe   (unsafePerformIO)
import           System.Process     hiding (env)
import           Text.Regex

import           MT5.Communication
import           MT5.Config
import           MT5.PyProc


venvPython :: Config -> IO FilePath
venvPython config = makeAbsolute (venvDir config ++ "/bin/python")


-- | Python process object, fetchable from within IO so we don't need to pass it to every function/hide it with Reader
mt5ServerThread :: IORef (ThreadId, Handle, Handle)
mt5ServerThread = unsafePerformIO $ newIORef (error "Not yet initialized" :: (ThreadId, Handle, Handle))
{-# NOINLINE mt5ServerThread #-}


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
-- Overwrites the global config.
startMT5 :: Config -> IO Config
startMT5 config = do
  cloneMT5Linux
  -- Check if this is a new installation by checking venv directory existence
  venvExists <- doesDirectoryExist (venvDir config)
  let newInstall = not venvExists
  -- Install Windows Python libraries first, then create/setup Linux venv
  config' <- installMT5InWine newInstall
  when newInstall createVenv
  startMT5Server config'
  threadDelay (1 * 10 ^ 6) -- give it some time to startup
  createPythonProcess config'
  setGlobalConfig config'
  return config'
  where
    -- | Helper function to locate executables using the locate command
    locateExecutable :: String -> IO String
    locateExecutable executable = do
      $(logInfoText) $ "Using program locate to find " <> T.pack executable
      (exitCode, stdOut, stdErr) <- readProcessWithExitCode "locate" [executable] []
      when (exitCode /= ExitSuccess) $ do
        $(logError) $ "locate " ++ executable ++ " failed with exit code: " ++ show exitCode
        $(logError) $ "stdout: " ++ stdOut
        $(logError) $ "stderr: " ++ stdErr
        error $ "Could not find " ++ executable ++ ". Make sure you have (i) wine installed, (ii) " ++ 
          executable ++ " installed in wine, and (iii) the locate database is up to date."
      $(logInfo) $ "locate " ++ executable ++ " succeeded: " ++ stdOut
      -- Parse the output to get the first executable path
      let executables = lines stdOut
      $(logInfo) $ "Found " ++ executable ++ ": " ++ show executables
      when (null executables) $ 
        error $ "No " ++ executable ++ " found in locate database"
      let noVenvExecutables = fromMaybe executables $ toMaybe (filter (not . T.isInfixOf "venv" . T.pack) executables)
      when (null noVenvExecutables) $
        error $ "All found " ++ executable ++ " instances are in virtual environments"
      return $ head $ sortOn (Down . getPythonVersion) noVenvExecutables
      where
        toMaybe :: [a] -> Maybe [a]
        toMaybe [] = Nothing
        toMaybe xs = Just xs
        getPythonVersion :: String -> String  
        getPythonVersion str = subRegex (mkRegex "^.*/([pP]ython[0-9]+)/.*") str "\\1"


    installMT5InWine :: Bool -> IO Config
    installMT5InWine newInstall = do
      -- Wrap the installation in a try-catch to clean up venv on error
      installAction `catch` handleError
      where
        installAction = do
          winPython <- locateExecutable "python.exe"
          winPip <- locateExecutable "pip.exe"
          $(logInfo) $ "Using Windows Python at " ++ winPython
          $(logInfo) $ "Using Windows pip at " ++ winPip
          when newInstall $ do
            pipInstall winPip "MetaTrader5"
            -- pipInstall winPip "json"
            -- pipUpgrade winPip "MetaTrader5"
            pipInstall winPip "-e /tmp/mt5linux/"
            -- pipInstall winPip "matplotlib"
            -- pipInstall winPip "pandas"
            -- pipInstall winPip "-r /tmp/mt5linux/requirements.txt"
          return $ config {winePython = winPython, winePip = winPip}
        
        handleError :: SomeException -> IO Config
        handleError e = do
          $(logError) $ "Error during MT5 Wine installation: " ++ show e
          $(logInfoText) $ "Cleaning up venv directory: " <> T.pack (venvDir config)
          venvExists <- doesDirectoryExist (venvDir config)
          when venvExists $ do
            $(logInfoText) "Removing venv directory to prevent inconsistent state"
            removeDirectoryRecursive (venvDir config)
          $(logInfoText) "Venv cleanup completed, rethrowing error"
          throwIO e
        
        pipInstall winPip name = pip winPip ("install " ++ name)
        pipUpgrade winPip name = pipInstall winPip ("--upgrade " ++ name)
        pip winPip cmd = do
          let fullCmd = "/usr/bin/wine " ++ winPip ++ " " ++ cmd
          $(logInfo) $ "Running pip command: " ++ fullCmd
          res <- spawnCommand fullCmd >>= waitForProcess
          when (res /= ExitSuccess) $ error $ "ERROR Cmd: '" ++ fullCmd ++ "' Result Code: " ++ show res

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
        unless exists $ do callProcess "git" ["clone", mt5linuxGitRepo config, "/tmp/mt5linux"]
    -- | Create venv and install libs. Only performs install if virtualenv is not initialized yet. Reinstall by
    -- deleting the folder [venvDir].
    createVenv :: IO ()
    createVenv = do
      pythons <-
        filter ('-' `notElem`) . filter (T.isInfixOf "python3." . T.pack) . lines <$>
        readProcess "ls" ["/usr/bin/"] ""
      when (null pythons) $
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
