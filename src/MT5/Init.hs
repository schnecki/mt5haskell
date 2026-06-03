{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module MT5.Init
    ( startMT5
    , stopMT5
    , mt5Started
    , detectExecutionEnvironment
    , detectPythonEnvironments
    , selectBestPythonEnvironment
    , autoDetectConfig
    , resolveMT5LinuxPath
    , setupMT5LinuxRepository
    , executePythonCommand
    , pipInstallWithEnv
    ) where

import           Control.Concurrent       (ThreadId, forkIO, killThread,
                                           threadDelay)
import           Control.Exception        (SomeException, catch, throwIO, try)
import           Control.Monad            (filterM, liftM2, unless, void, when,
                                           (<=<))
import qualified Data.ByteString          as B
import           Data.IORef
import           Data.List                (isPrefixOf, sortOn)
import           Data.Maybe               (fromMaybe, isNothing)
import           Data.Ord                 (Down (..))
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as Encoding
import           EasyLogger
import           GHC.IO
import           GHC.IO.Handle
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           getDirectoryContents, makeAbsolute,
                                           removeDirectoryRecursive, removeFile)
import           System.Exit
import           System.FilePath          ((</>))
import           System.IO
import           System.Process           hiding (env)
import           Text.Regex

import           MT5.API
import           MT5.Communication
import           MT5.Communication.PyProc
import           MT5.Communication.Socket
import           MT5.Config


venvPython :: Config -> IO FilePath
venvPython config = makeAbsolute (venvDir config ++ "/bin/python")

mt5Started :: IORef Bool
mt5Started = unsafePerformIO $ newIORef False
{-# NOINLINE mt5Started #-}

-- | Handle of the RPyC server thread (only meaningful for the daemon owner).
mt5ServerThread :: IORef (ThreadId, Handle, Handle)
mt5ServerThread = unsafePerformIO $ newIORef (error "Not yet initialized" :: (ThreadId, Handle, Handle))
{-# NOINLINE mt5ServerThread #-}

-- | ProcessHandle of the socket daemon (only meaningful for the daemon owner).
mt5DaemonProcess :: IORef (Maybe ProcessHandle)
mt5DaemonProcess = unsafePerformIO $ newIORef Nothing
{-# NOINLINE mt5DaemonProcess #-}


-- =====================================================================
-- Socket daemon lifecycle
-- =====================================================================

-- | Start the Python socket daemon in the background.
--
-- The daemon binds to 'socketPath', initialises MT5, and accepts
-- connections from any number of Haskell clients.
startSocketDaemon :: Config -> IO ()
startSocketDaemon config = do
    python <- venvPython config
    let pythonPath = venvDir config </> "python_server.py"
    writeFile pythonPath (T.unpack (Encoding.decodeLatin1 pythonCode))
    devNull <- openFile "/dev/null" ReadWriteMode
    (_, _, _, pHandle) <-
        createProcess (proc python [pythonPath])
            { cwd    = Just "."
            , std_in  = UseHandle devNull
            , std_out = UseHandle devNull
            , std_err = UseHandle devNull
            }
    writeIORef mt5DaemonProcess (Just pHandle)

-- | Block until the daemon socket is connectable, up to @n@ seconds.
waitForSocket :: Int -> IO ()
waitForSocket 0 = error "MT5 socket daemon did not start within the timeout"
waitForSocket n = do
    avail <- isSocketAvailable
    unless avail $ threadDelay (1 * 10^(6 :: Int)) >> waitForSocket (n - 1)

-- | Open a connection to the running daemon and store it in 'pyProc'.
connectToDaemon :: IO ()
connectToDaemon = do
    h <- connectSocketHandle
    writeIORef pyProc $ Just $ PyProc h h (hClose h)


-- =====================================================================
-- Environment Detection Functions
-- =====================================================================

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = fmap (filter (`notElem` [".", ".."])) (getDirectoryContents path)

locateExecutable :: String -> IO String
locateExecutable executable = do
    $(logInfoText) $ "Using program locate to find " <> T.pack executable
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode "locate" [executable] []
    when (exitCode /= ExitSuccess) $ do
        $(logError) $ "locate " ++ executable ++ " failed with exit code: " ++ show exitCode
        $(logError) $ "stdout: " ++ stdOut
        $(logError) $ "stderr: " ++ stdErr
        error $ "Could not find " ++ executable ++ ". Make sure you have (i) wine installed, (ii) " ++
            executable ++ " installed in wine, and (iii) the locate database is up to date. Update with: sudo updatedb"
    $(logInfo) $ "locate " ++ executable ++ " succeeded: " ++ stdOut
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
        hasMntC <- doesDirectoryExist "/mnt/c"
        hasWslExe <- doesDirectoryExist "/mnt/c/Windows/System32"
        return (hasMntC && hasWslExe)
    detectWine = do
        (exitCode, _, _) <- readProcessWithExitCode "which" ["wine"] ""
        return (exitCode == ExitSuccess)

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
        validPythons <- filterM validatePythonPath pythonPaths
        return $ map createDirectPythonEnv validPythons

    detectWinePython = do
        pythonPath <- locateExecutable "python.exe"
        pipPath <- locateExecutable "pip.exe"
        return [PythonEnvironment pythonPath pipPath WineExecution]
        `catch` \(_ :: SomeException) -> return []

    findWSLPythonPaths = do
        let basePaths = [ "/mnt/c/Users", "/mnt/c/Python38", "/mnt/c/Python39"
                        , "/mnt/c/Python310", "/mnt/c/Python311"
                        , "/mnt/c/Program Files/Python38", "/mnt/c/Program Files/Python39"
                        , "/mnt/c/Program Files/Python310", "/mnt/c/Program Files/Python311"
                        ]
        foundPaths <- filterM doesDirectoryExist basePaths
        concatMapM findPythonInPath foundPaths

    findPythonInPath basePath = do
        if "/mnt/c/Users" `isPrefixOf` basePath
            then do
                userDirs <- listDirectory basePath
                let pythonDirs = map (\user -> basePath </> user </> "AppData/Local/Programs/Python") userDirs
                existingDirs <- filterM doesDirectoryExist pythonDirs
                concatMapM findPythonExecutables existingDirs
            else findPythonExecutables basePath

    findPythonExecutables dir = do
        let directPython = dir </> "python.exe"
        let directPip    = dir </> "Scripts/pip.exe"
        directExists <- liftM2 (&&) (doesFileExist directPython) (doesFileExist directPip)
        subDirs <- getDirectoryContents dir `catch` \(_ :: SomeException) -> return []
        let versionDirs = filter (\d -> d `notElem` [".", ".."] && "Python" `isPrefixOf` d) subDirs
        versionResults <- concatMapM (\vDir -> do
            let versionPath   = dir </> vDir
            let versionPython = versionPath </> "python.exe"
            let versionPip    = versionPath </> "Scripts/pip.exe"
            versionExists <- liftM2 (&&) (doesFileExist versionPython) (doesFileExist versionPip)
            if versionExists then return [(versionPython, versionPip)] else return []
            ) versionDirs
        let directResult = if directExists then [(directPython, directPip)] else []
        return (directResult ++ versionResults)

    validatePythonPath (pythonPath, _) = do
        result <- readProcessWithExitCode pythonPath ["--version"] ""
                  `catch` \(_ :: SomeException) -> return (ExitFailure 1, "", "")
        let (exitCode, _, _) = result
        return (exitCode == ExitSuccess)

    createDirectPythonEnv (pythonPath, pipPath) =
        PythonEnvironment pythonPath pipPath DirectExecution

selectBestPythonEnvironment :: [PythonEnvironment] -> Maybe ExecutionMode -> Maybe PythonEnvironment
selectBestPythonEnvironment [] _ = Nothing
selectBestPythonEnvironment envs Nothing = Just (head envs)
selectBestPythonEnvironment envs (Just preferredMode) =
    case filter (\env -> executionMode env == preferredMode) envs of
        []            -> Just (head envs)
        (preferred:_) -> Just preferred

autoDetectConfig :: Config -> IO Config
autoDetectConfig config = do
    env <- detectExecutionEnvironment
    $(logInfo) $ "Detected execution environment: " ++ show env
    pythonEnvs <- detectPythonEnvironments
    let selectedEnv = selectBestPythonEnvironment pythonEnvs (preferredMode config)
    let (winePython', winePip') = case selectedEnv of
            Just (PythonEnvironment python pip _) -> (python, pip)
            Nothing -> (winePython config, winePip config)
    return config
        { executionEnv = Just env
        , pythonEnv    = selectedEnv
        , winePython   = winePython'
        , winePip      = winePip'
        }


-- =====================================================================
-- Repository Management Functions
-- =====================================================================

resolveMT5LinuxPath :: Config -> IO FilePath
resolveMT5LinuxPath config = case mt5linuxLocalPath config of
    Just userPath -> do
        exists <- doesDirectoryExist userPath
        if exists
            then makeAbsolute userPath
            else error $ "Specified mt5linux path does not exist: " ++ userPath
    Nothing -> return "/tmp/mt5linux"

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
        let requiredFiles = ["setup.py", "mt5linux/__init__.py"]
        missing <- filterM (fmap not . doesFileExist . (path </>)) requiredFiles
        unless (null missing) $
            error $ "Invalid mt5linux repository, missing files: " ++ show missing


-- =====================================================================
-- Command Execution Abstraction
-- =====================================================================

executePythonCommand :: PythonEnvironment -> String -> [String] -> IO ExitCode
executePythonCommand env cmd args = case executionMode env of
    WineExecution -> do
        let fullCmd = "/usr/bin/wine " ++ cmd
        spawnCommand (unwords (fullCmd : args)) >>= waitForProcess
    DirectExecution ->
        spawnCommand (unwords (cmd : args)) >>= waitForProcess

pipInstallWithEnv :: PythonEnvironment -> FilePath -> String -> IO ()
pipInstallWithEnv env repoPath package = do
    windowsPath <- if "-e " `isPrefixOf` package && executionMode env == DirectExecution
                       then do
                           (exitCode, stdout, _) <- readProcessWithExitCode "wslpath" ["-w", repoPath] ""
                           if exitCode == ExitSuccess
                               then return $ map (\c -> if c == '\\' then '/' else c) (strip stdout)
                               else return repoPath
                       else return repoPath
    let (args, packageDesc) = if "-e " `isPrefixOf` package
                                  then (["install", "-e", windowsPath], package ++ " from " ++ windowsPath)
                                  else (["install", package], package)
    $(logInfo) $ "Installing package: " ++ packageDesc ++ " using " ++ show (executionMode env)
    exitCode <- executePythonCommand env (pipExecutable env) args
    when (exitCode /= ExitSuccess) $
        error $ "Failed to install package: " ++ package
  where
    strip :: String -> String
    strip = reverse . dropWhile (`elem` (" \t\r\n" :: String)) . reverse . dropWhile (`elem` (" \t\r\n" :: String))


-- =====================================================================
-- Main Initialization
-- =====================================================================

-- | Start MT5 or connect to an already-running daemon.
--
-- If the daemon socket at 'socketPath' is already reachable, this process
-- simply connects as a client.  Otherwise it sets up the venv (if needed),
-- starts the RPyC server and the socket daemon, waits for the socket to
-- appear, then connects.
startMT5 :: Config -> IO Config
startMT5 config = do
    available <- isSocketAvailable
    if available
        then do
            -- Another instance already owns the daemon; just connect.
            $(logInfoText) "MT5 daemon already running — connecting as client"
            connectToDaemon
            writeIORef mt5DaemonOwner False
            writeIORef mt5Started True
            return config
        else do
            writeIORef mt5DaemonOwner True
            startDaemonOwner config
  where
    startDaemonOwner :: Config -> IO Config
    startDaemonOwner cfg = do
        config' <- if isNothing (executionEnv cfg) || isNothing (pythonEnv cfg)
                       then autoDetectConfig cfg
                       else return cfg

        repoPath <- setupMT5LinuxRepository config'
        let config'' = config' { mt5linuxLocalPath = Just repoPath }

        venvExists <- doesDirectoryExist (venvDir config'')
        pythonBin  <- venvPython config''
        venvValid  <- if venvExists then doesFileExist pythonBin else return False
        when (venvExists && not venvValid) $ do
            $(logInfo) $ "Stale venv detected, removing: " ++ pythonBin
            removeDirectoryRecursive (venvDir config'')
        let newInstall = not venvExists || not venvValid

        config''' <- setupPythonEnvironment config'' newInstall
        when newInstall createVenv

        startMT5Server config'''
        threadDelay (1 * 10^(6 :: Int))

        startSocketDaemon config'''
        $(logInfoText) "Waiting for MT5 socket daemon to become ready"
        waitForSocket 15

        connectToDaemon
        writeIORef mt5Started True
        return config'''
      where
        setupPythonEnvironment :: Config -> Bool -> IO Config
        setupPythonEnvironment c newInstall =
            case pythonEnv c of
                Just env -> setupWithDetectedEnv c env newInstall
                Nothing  -> installMT5InWine c newInstall

        setupWithDetectedEnv :: Config -> PythonEnvironment -> Bool -> IO Config
        setupWithDetectedEnv c env newInstall = do
            repoPath <- resolveMT5LinuxPath c
            when newInstall $ do
                pipInstallWithEnv env repoPath "MetaTrader5"
                pipInstallWithEnv env repoPath "-e ."
            return c

        installMT5InWine :: Config -> Bool -> IO Config
        installMT5InWine c newInstall =
            installAction `catch` handleError
          where
            installAction = do
                winPython <- locateExecutable "python.exe"
                winPip    <- locateExecutable "pip.exe"
                when newInstall $ do
                    pipInstall winPython winPip "MetaTrader5"
                    pipInstall winPython winPip "-e /tmp/mt5linux/"
                return $ c { winePython = winPython, winePip = winPip }

            handleError :: SomeException -> IO Config
            handleError e = do
                $(logError) $ "Error during MT5 Wine installation: " ++ show e
                venvExists <- doesDirectoryExist (venvDir c)
                when venvExists $ removeDirectoryRecursive (venvDir c)
                throwIO e

            pipInstall winPython winPip name = pip winPython winPip ("install " ++ name)
            pip winPython winPip cmd = do
                let wineEnv = PythonEnvironment winPython winPip WineExecution
                exitCode <- executePythonCommand wineEnv winPip (words cmd)
                when (exitCode /= ExitSuccess) $
                    error $ "ERROR pip " ++ cmd ++ " failed with exit code: " ++ show exitCode

        startMT5Server :: Config -> IO ()
        startMT5Server c = do
            when (winePython c == "") $ error "Need to call installMT5InWine before startMT5Server"
            python <- venvPython c
            devNullRead  <- openFile "/dev/null" ReadMode
            devNullWrite <- openFile "/dev/null" WriteMode
            threadId <- forkIO $ void $
                runProcess python ["-m", "mt5linux", winePython c]
                    Nothing Nothing
                    (Just devNullRead) (Just devNullWrite) (Just devNullWrite)
            $(logInfo) $ "Successfully started mt5linux server. ThreadId: " <> show threadId
            writeIORef mt5ServerThread (threadId, devNullRead, devNullWrite)

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


-- | Disconnect from the daemon (and shut it down if this is the owning process).
stopMT5 :: IO ()
stopMT5 = do
    mPp <- readIORef pyProc
    owner <- readIORef mt5DaemonOwner
    case mPp of
        Nothing -> return ()
        Just pp -> do
            when owner $ do
                -- Ask daemon to shut down MT5 and exit.
                send "SHUTDOWN"
                void $ (try @SomeException) (receive >>= B.putStr)
            pyCleanup pp
    when owner $ do
        -- Stop the RPyC server thread.
        (threadId, devNullRead, devNullWrite) <- readIORef mt5ServerThread
        killThread threadId
        hClose devNullRead
        hClose devNullWrite
        -- Kill the daemon process (defensive; SHUTDOWN should have exited it).
        mDaemon <- readIORef mt5DaemonProcess
        case mDaemon of
            Nothing -> return ()
            Just ph -> terminateProcess ph
        -- Remove stale socket file.
        sockExists <- doesFileExist socketPath
        when sockExists $ removeFile socketPath
