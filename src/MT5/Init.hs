{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module MT5.Init
    ( startMT5
    , stopMT5
    , detectExecutionEnvironment
    , detectPythonEnvironments
    , selectBestPythonEnvironment
    , autoDetectConfig
    , resolveMT5LinuxPath
    , setupMT5LinuxRepository
    , executePythonCommand
    , pipInstallWithEnv
    ) where

import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Exception  (SomeException, catch, throwIO)
import           Control.Monad      (filterM, liftM2, unless, void, when)
import qualified Data.ByteString    as B
import           Data.IORef
import           Data.List          (isPrefixOf, sortOn)
import           Data.Maybe         (fromMaybe, isNothing)
import           Data.Ord           (Down (..))
import qualified Data.Text          as T
import           EasyLogger
import           GHC.IO
import           GHC.IO.Handle
import           System.Directory   (doesDirectoryExist, doesFileExist,
                                     getDirectoryContents, makeAbsolute,
                                     removeDirectoryRecursive)
import           System.Exit
import           System.FilePath    ((</>))
import           System.IO
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

-- =====================================================================
-- Environment Detection Functions
-- =====================================================================

-- | Helper function similar to whenM from Control.Monad.Extra
whenM :: Monad m => m Bool -> m () -> m ()
whenM condM action = do
  cond <- condM
  when cond action

-- | Helper function for concatenating results of mapM
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | List directory contents (excluding . and ..)
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = fmap (filter (`notElem` [".", ".."])) (getDirectoryContents path)

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

-- | Detect current execution environment
detectExecutionEnvironment :: IO ExecutionEnvironment
detectExecutionEnvironment = do
  isWSL <- detectWSL
  hasWine <- detectWine
  if isWSL
    then return WSLEnvironment
    else if hasWine
           then return WineEnvironment
           else return NativeLinux
  where
    detectWSL = do
      -- Check for WSL indicators
      hasMntC <- doesDirectoryExist "/mnt/c"
      hasWslExe <- doesDirectoryExist "/mnt/c/Windows/System32"
      return (hasMntC && hasWslExe)

    detectWine = do
      -- Check if wine command is available
      (exitCode, _, _) <- readProcessWithExitCode "which" ["wine"] ""
      return (exitCode == ExitSuccess)

-- | Detect available Python environments
detectPythonEnvironments :: IO [PythonEnvironment]
detectPythonEnvironments = do
  env <- detectExecutionEnvironment
  case env of
    WSLEnvironment  -> detectWSLPython
    WineEnvironment -> detectWinePython
    NativeLinux     -> return []
  where
    detectWSLPython = do
      $(logInfo) $ T.pack "Starting WSL Python detection"
      pythonPaths <- findWSLPythonPaths
      $(logInfo) $ "Found " ++ show (length pythonPaths) ++ " potential Python paths"
      mapM_ (\(python, pip) -> $(logInfo) $ "  Python path: " ++ python ++ ", Pip path: " ++ pip) pythonPaths
      validPythons <- filterM validatePythonPath pythonPaths
      $(logInfo) $ "Validated " ++ show (length validPythons) ++ " Python environments"
      mapM_ (\(python, pip) -> $(logInfo) $ "  Valid Python: " ++ python ++ ", Pip: " ++ pip) validPythons
      return $ map createDirectPythonEnv validPythons

    detectWinePython = do
      -- Use existing locateExecutable approach
      pythonPath <- locateExecutable "python.exe"
      pipPath <- locateExecutable "pip.exe"
      return [PythonEnvironment pythonPath pipPath WineExecution]
      `catch` \(_ :: SomeException) -> return []

    findWSLPythonPaths = do
      -- Search common Windows Python installation paths
      let basePaths = [ "/mnt/c/Users"
                      , "/mnt/c/Python38"
                      , "/mnt/c/Python39"
                      , "/mnt/c/Python310"
                      , "/mnt/c/Python311"
                      , "/mnt/c/Program Files/Python38"
                      , "/mnt/c/Program Files/Python39"
                      , "/mnt/c/Program Files/Python310"
                      , "/mnt/c/Program Files/Python311"
                      ]
      $(logInfo) $ "Searching in base paths: " ++ show basePaths
      foundPaths <- filterM doesDirectoryExist basePaths
      $(logInfo) $ "Found existing base directories: " ++ show foundPaths
      concatMapM findPythonInPath foundPaths

    findPythonInPath basePath = do
      $(logInfo) $ "Searching for Python in path: " ++ basePath
      if "/mnt/c/Users" `isPrefixOf` basePath
        then do
          -- Search in user directories
          $(logInfo) $ "Searching in user directories under: " ++ basePath
          userDirs <- listDirectory basePath
          $(logInfo) $ "Found user directories: " ++ show userDirs
          let pythonDirs = map (\user -> basePath </> user </> "AppData/Local/Programs/Python") userDirs
          $(logInfo) $ "Checking Python directories: " ++ show pythonDirs
          existingDirs <- filterM doesDirectoryExist pythonDirs
          $(logInfo) $ "Found existing Python directories: " ++ show existingDirs
          concatMapM findPythonExecutables existingDirs
        else findPythonExecutables basePath

    findPythonExecutables dir = do
      $(logInfo) $ "Searching for executables in: " ++ dir
      -- Check if this directory directly contains python.exe (for direct installs)
      let directPython = dir </> "python.exe"
      let directPip = dir </> "Scripts/pip.exe"
      directExists <- liftM2 (&&) (doesFileExist directPython) (doesFileExist directPip)

      -- Also check version subdirectories (for Windows Python installs)
      subDirs <- getDirectoryContents dir
        `catch` \(_ :: SomeException) -> return []
      let versionDirs = filter (\d -> not (d `elem` [".", ".."]) && "Python" `isPrefixOf` d) subDirs
      $(logInfo) $ "Found potential Python version directories: " ++ show versionDirs

      versionResults <- concatMapM (\vDir -> do
        let versionPath = dir </> vDir
        let versionPython = versionPath </> "python.exe"
        let versionPip = versionPath </> "Scripts/pip.exe"
        $(logInfo) $ "Checking version path: " ++ versionPath
        versionExists <- liftM2 (&&) (doesFileExist versionPython) (doesFileExist versionPip)
        if versionExists
          then do
            $(logInfo) $ "Found valid Python installation at: " ++ versionPath
            return [(versionPython, versionPip)]
          else return []
        ) versionDirs

      let directResult = if directExists then [(directPython, directPip)] else []
      let allResults = directResult ++ versionResults
      $(logInfo) $ "Total Python installations found in " ++ dir ++ ": " ++ show (length allResults)
      return allResults

    validatePythonPath (pythonPath, pipPath) = do
      -- Test if Python can be executed (Windows exe in WSL can be called directly)
      $(logInfo) $ "Validating Python path: " ++ pythonPath
      result <- (readProcessWithExitCode pythonPath ["--version"] ""
        `catch` \(e :: SomeException) -> do
          $(logInfo) $ "Exception during validation: " ++ show e
          return (ExitFailure 1, "", ""))
      let (exitCode, stdout, stderr) = result
      $(logInfo) $ "Validation result for " ++ pythonPath ++ ": " ++ show exitCode ++ ", stdout: " ++ show stdout ++ ", stderr: " ++ show stderr
      return (exitCode == ExitSuccess)

    createDirectPythonEnv (pythonPath, pipPath) =
      PythonEnvironment pythonPath pipPath DirectExecution

-- | Choose best Python environment based on availability and preferences
selectBestPythonEnvironment :: [PythonEnvironment] -> Maybe ExecutionMode -> Maybe PythonEnvironment
selectBestPythonEnvironment [] _ = Nothing
selectBestPythonEnvironment envs Nothing = Just (head envs)  -- Take first available
selectBestPythonEnvironment envs (Just preferredMode) =
  case filter (\env -> executionMode env == preferredMode) envs of
    []            -> Just (head envs)  -- Fallback to first available if preferred not found
    (preferred:_) -> Just preferred

-- | Auto-detect and create optimal configuration
autoDetectConfig :: Config -> IO Config
autoDetectConfig config = do
  env <- detectExecutionEnvironment
  $(logInfo) $ "Detected execution environment: " ++ show env
  pythonEnvs <- detectPythonEnvironments
  $(logInfo) $ "Detected Python environments: " ++ show (length pythonEnvs) ++ " environments"
  mapM_ (\penv -> $(logInfo) $ "  Environment: " ++ show penv) pythonEnvs
  let selectedEnv = selectBestPythonEnvironment pythonEnvs (preferredMode config)
  $(logInfo) $ "Selected Python environment: " ++ show selectedEnv

  -- Update legacy fields for backward compatibility
  let (winePython', winePip') = case selectedEnv of
        Just (PythonEnvironment python pip _) -> (python, pip)
        Nothing -> (winePython config, winePip config)

  return config
    { executionEnv = Just env
    , pythonEnv = selectedEnv
    , winePython = winePython'
    , winePip = winePip'
    }

-- =====================================================================
-- Repository Management Functions
-- =====================================================================

-- | Determine mt5linux repository path based on config
resolveMT5LinuxPath :: Config -> IO FilePath
resolveMT5LinuxPath config = case mt5linuxLocalPath config of
  Just userPath -> do
    exists <- doesDirectoryExist userPath
    if exists
      then makeAbsolute userPath
      else error $ "Specified mt5linux path does not exist: " ++ userPath
  Nothing -> return "/tmp/mt5linux"  -- Default fallback

-- | Setup mt5linux repository (clone or use existing)
setupMT5LinuxRepository :: Config -> IO FilePath
setupMT5LinuxRepository config = do
  repoPath <- resolveMT5LinuxPath config
  exists <- doesDirectoryExist repoPath

  if exists
    then do
      $(logInfo) $ "Using existing mt5linux repository at: " ++ repoPath
      validateRepository repoPath
      return repoPath
    else do
      $(logInfo) $ "Cloning mt5linux repository to: " ++ repoPath
      callProcess "git" ["clone", mt5linuxGitRepo config, repoPath]
      return repoPath
  where
    validateRepository path = do
      -- Check for required files
      let requiredFiles = ["setup.py", "mt5linux/__init__.py"]
      missing <- filterM (fmap not . doesFileExist . (path </>)) requiredFiles
      unless (null missing) $
        error $ "Invalid mt5linux repository, missing files: " ++ show missing

-- =====================================================================
-- Command Execution Abstraction
-- =====================================================================

-- | Execute Python/pip commands using appropriate method
executePythonCommand :: PythonEnvironment -> String -> [String] -> IO ExitCode
executePythonCommand env cmd args = case executionMode env of
  WineExecution -> do
    let fullCmd = "/usr/bin/wine " ++ cmd
    spawnCommand (unwords (fullCmd : args)) >>= waitForProcess
  DirectExecution ->
    spawnCommand (unwords (cmd : args)) >>= waitForProcess

-- | Execute pip install with environment-appropriate method
pipInstallWithEnv :: PythonEnvironment -> FilePath -> String -> IO ()
pipInstallWithEnv env repoPath package = do
  -- For editable installs with DirectExecution (WSL), convert Linux path to Windows format
  windowsPath <- if "-e " `isPrefixOf` package && executionMode env == DirectExecution
                   then do
                     (exitCode, stdout, _) <- readProcessWithExitCode "wslpath" ["-w", repoPath] ""
                     if exitCode == ExitSuccess
                       then return $ map (\c -> if c == '\\' then '/' else c) (strip stdout)  -- Convert backslashes to forward slashes
                       else return repoPath  -- Fallback to original path
                   else return repoPath

  let (args, packageDesc) = if "-e " `isPrefixOf` package
                              then (["install", "-e", windowsPath], package ++ " from " ++ windowsPath)  -- For editable installs
                              else (["install", package], package)                                       -- For normal packages
  $(logInfo) $ "Installing package: " ++ packageDesc ++ " using " ++ show (executionMode env)
  exitCode <- executePythonCommand env (pipExecutable env) args
  when (exitCode /= ExitSuccess) $
    error $ "Failed to install package: " ++ package
  where
    strip :: String -> String
    strip = reverse . dropWhile (`elem` (" \t\r\n" :: String)) . reverse . dropWhile (`elem` (" \t\r\n" :: String))

-- =====================================================================
-- Updated Initialization Functions
-- =====================================================================

-- | Clones the repo, installs the libraries on linux and windows side and starts the server and python process.
-- Overwrites the global config.
startMT5 :: Config -> IO Config
startMT5 config = do
  -- 1. Auto-detect environment if not specified
  config' <- if isNothing (executionEnv config) || isNothing (pythonEnv config)
               then autoDetectConfig config
               else return config

  -- 2. Setup repository (clone or use existing)
  repoPath <- setupMT5LinuxRepository config'
  let config'' = config' { mt5linuxLocalPath = Just repoPath }

  -- Check if this is a new installation by checking venv directory existence
  venvExists <- doesDirectoryExist (venvDir config'')
  let newInstall = not venvExists

  -- Install Windows Python libraries first, then create/setup Linux venv
  config''' <- setupPythonEnvironment config'' newInstall
  when newInstall createVenv
  startMT5Server config'''
  threadDelay (1 * 10 ^ 6) -- give it some time to startup
  createPythonProcess config'''
  return config'''
  where
    setupPythonEnvironment :: Config -> Bool -> IO Config
    setupPythonEnvironment cfg newInstall =
      case pythonEnv cfg of
        Just env -> do
          $(logInfo) $ "Using detected Python environment: " ++ show (executionMode env)
          setupWithDetectedEnv cfg env newInstall
        Nothing -> do
          $(logInfo) $ T.pack "No Python environment detected, falling back to Wine installation"
          installMT5InWine cfg newInstall  -- Fallback to Wine

    setupWithDetectedEnv :: Config -> PythonEnvironment -> Bool -> IO Config
    setupWithDetectedEnv cfg env newInstall = do
      repoPath <- resolveMT5LinuxPath cfg
      when newInstall $ do
        pipInstallWithEnv env repoPath "MetaTrader5"
        pipInstallWithEnv env repoPath "-e ."
      return cfg

    installMT5InWine :: Config -> Bool -> IO Config
    installMT5InWine cfg newInstall = do
      -- Wrap the installation in a try-catch to clean up venv on error
      installAction `catch` handleError
      where
        installAction = do
          winPython <- locateExecutable "python.exe"
          winPip <- locateExecutable "pip.exe"
          $(logInfo) $ "Using Windows Python at " ++ winPython
          $(logInfo) $ "Using Windows pip at " ++ winPip
          when newInstall $ do
            pipInstall winPython winPip "MetaTrader5"
            -- pipInstall winPython winPip "json"
            -- pipUpgrade winPython winPip "MetaTrader5"
            pipInstall winPython winPip "-e /tmp/mt5linux/"
            -- pipInstall winPython winPip "matplotlib"
            -- pipInstall winPython winPip "pandas"
            -- pipInstall winPython winPip "-r /tmp/mt5linux/requirements.txt"
          return $ cfg {winePython = winPython, winePip = winPip}

        handleError :: SomeException -> IO Config
        handleError e = do
          $(logError) $ "Error during MT5 Wine installation: " ++ show e
          $(logInfoText) $ "Cleaning up venv directory: " <> T.pack (venvDir cfg)
          venvExists <- doesDirectoryExist (venvDir cfg)
          when venvExists $ do
            $(logInfoText) "Removing venv directory to prevent inconsistent state"
            removeDirectoryRecursive (venvDir cfg)
          $(logInfoText) "Venv cleanup completed, rethrowing error"
          throwIO e

        pipInstall winPython winPip name = pip winPython winPip ("install " ++ name)
        pipUpgrade winPython winPip name = pipInstall winPython winPip ("--upgrade " ++ name)
        pip winPython winPip cmd = do
          -- Create a Wine environment for consistent execution
          let wineEnv = PythonEnvironment winPython winPip WineExecution
          exitCode <- executePythonCommand wineEnv winPip (words cmd)
          when (exitCode /= ExitSuccess) $
            error $ "ERROR pip " ++ cmd ++ " failed with exit code: " ++ show exitCode

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
      repoPath <- resolveMT5LinuxPath config
      pipInstall ("-e " ++ repoPath)
      where
        pipInstall name = pip ("install " ++ name)
        pip cmd = do
          res <- spawnCommand (venvDir config ++ "/bin/pip " ++ cmd) >>= waitForProcess
          when (res /= ExitSuccess) $ error $ "ERROR: Could not run pip " ++ cmd ++ " Code: " ++ show res


-- | Stopping the processes
stopMT5 :: IO ()
stopMT5 = do
  send "QUIT" >> receive >>= B.putStr
  putStrLn ""
  threadDelay (1 * 10^6)
  (threadId, devNullRead, devNullWrite) <- readIORef mt5ServerThread
  killThread threadId
  hClose devNullRead
  hClose devNullWrite
  PyProc inp out pHandle <- readIORef pyProc
  cleanupProcess (Just inp, Just out, Nothing, pHandle)
