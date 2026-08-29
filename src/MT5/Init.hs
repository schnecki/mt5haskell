{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module MT5.Init
    ( startMT5
    , stopMT5
    , mt5Started
    , isSocketAvailable
    , detectExecutionEnvironment
    , detectPythonEnvironments
    , selectBestPythonEnvironment
    , autoDetectConfig
    , resolveMT5LinuxPath
    , setupMT5LinuxRepository
    , executePythonCommand
    , pipInstallWithEnv
    ) where

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (SomeException, catch, onException,
                                           throwIO, try)
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
import           GHC.IO         hiding (onException)
import           GHC.IO.Handle
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           getDirectoryContents, makeAbsolute,
                                           removeDirectoryRecursive, removeFile)
import           System.Exit
import           System.FilePath          ((</>))
import           System.IO
import           System.Posix.Files       (ownerReadMode, ownerWriteMode,
                                           unionFileModes)
import           System.Posix.IO          (OpenFileFlags (..), OpenMode (..),
                                           closeFd, defaultFileFlags, fdWrite,
                                           openFd)
import           System.Posix.Process     (getProcessID)
import           System.Process           hiding (env)
import           System.Timeout           (timeout)
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

-- | ProcessHandle of the RPyC server (only meaningful for the daemon owner).
mt5RpycProcess :: IORef (Maybe ProcessHandle)
mt5RpycProcess = unsafePerformIO $ newIORef Nothing
{-# NOINLINE mt5RpycProcess #-}

-- | ProcessHandle of the socket daemon (only meaningful for the daemon owner).
mt5DaemonProcess :: IORef (Maybe ProcessHandle)
mt5DaemonProcess = unsafePerformIO $ newIORef Nothing
{-# NOINLINE mt5DaemonProcess #-}


-- =====================================================================
-- Inter-process daemon ownership lock
-- =====================================================================

-- | Fixed path for the atomic daemon ownership lock file.
daemonLockPath :: FilePath
daemonLockPath = "/tmp/mt5haskell-daemon.lock"

-- | Atomically attempt to acquire daemon ownership.
--
-- Uses O_CREAT|O_EXCL so the create is atomic on POSIX systems.
-- Returns 'True' when this process is now the owner; 'False' when another
-- live process already holds the lock.  Stale locks (dead PID) are removed
-- and re-tried automatically.
tryAcquireDaemonLock :: IO Bool
tryAcquireDaemonLock = do
    pid <- show <$> getProcessID
    let flags = defaultFileFlags
            { exclusive = True
            , creat     = Just (ownerReadMode `unionFileModes` ownerWriteMode)
            }
    result <- try @IOError $ openFd daemonLockPath WriteOnly flags
    case result of
        Right fd -> do
            void $ fdWrite fd (pid ++ "\n")
            closeFd fd
            return True
        Left _ -> handleExistingLock
  where
    handleExistingLock :: IO Bool
    handleExistingLock = do
        content <- readFile daemonLockPath `catch` \(_ :: IOError) -> return ""
        let existingPid = stripWS content
        if null existingPid
            then retryAfterRemove
            else do
                alive <- doesFileExist ("/proc/" ++ existingPid)
                if alive then return False else retryAfterRemove

    retryAfterRemove :: IO Bool
    retryAfterRemove = do
        removeFile daemonLockPath `catch` \(_ :: IOError) -> return ()
        tryAcquireDaemonLock

-- | Release daemon ownership by removing the lock file.
releaseDaemonLock :: IO ()
releaseDaemonLock =
    removeFile daemonLockPath `catch` \(_ :: IOError) -> return ()

-- | Strip leading and trailing whitespace.
stripWS :: String -> String
stripWS = reverse . dropWhile (`elem` (" \t\r\n" :: String)) . reverse
        . dropWhile (`elem` (" \t\r\n" :: String))


-- =====================================================================
-- Socket daemon lifecycle
-- =====================================================================

-- | Start the Python socket daemon in the background.
--
-- The daemon binds to 'socketPath', initialises MT5, and accepts
-- connections from any number of Haskell clients.  When the last client
-- disconnects the daemon sends SIGTERM to @mRpycPid@ (if given) before
-- exiting, so the RPyC server is cleaned up automatically.
startSocketDaemon :: Config -> Maybe Pid -> IO ()
startSocketDaemon config mRpycPid = do
    python <- venvPython config
    let pythonPath = venvDir config </> "python_server.py"
    writeFile pythonPath (T.unpack (Encoding.decodeLatin1 pythonCode))
    devNull <- openFile "/dev/null" ReadWriteMode
    -- Capture the daemon's stdout/stderr in a persistent file instead of
    -- discarding to /dev/null.  The daemon's own structured '_log' output and
    -- any Python traceback land here, giving the only window into a stalled or
    -- crashing MT5 transport (the socket itself carries no diagnostics).
    let logPath = venvDir config </> "python_server.log"
    logHandle <- openFile logPath AppendMode
    hSetBuffering logHandle LineBuffering
    $(logInfo) ("MT5 daemon log: " ++ logPath)
    let rpycArgs = maybe [] (\pid -> [show pid]) mRpycPid
    (_, _, _, pHandle) <-
        createProcess (proc python (pythonPath : rpycArgs))
            { cwd    = Just "."
            , std_in  = UseHandle devNull
            , std_out = UseHandle logHandle
            , std_err = UseHandle logHandle
            }
    writeIORef mt5DaemonProcess (Just pHandle)

-- | Block until the daemon socket is connectable, up to @n@ seconds.
--
-- Throws an error when the timeout expires (used on the owner path where
-- failure means the daemon itself is broken).
waitForSocket :: Int -> IO ()
waitForSocket 0 = error "MT5 socket daemon did not start within the timeout"
waitForSocket n = do
    avail <- isSocketAvailable
    unless avail $ threadDelay (1 * 10^(6 :: Int)) >> waitForSocket (n - 1)

-- | Non-throwing variant: returns 'False' when the socket does not appear
-- within @n@ seconds.  Used on the client path where we want to fall back.
waitForSocketTimeout :: Int -> IO Bool
waitForSocketTimeout 0 = return False
waitForSocketTimeout n = do
    avail <- isSocketAvailable
    if avail
        then return True
        else threadDelay (1 * 10^(6 :: Int)) >> waitForSocketTimeout (n - 1)

-- | Open a connection to the running daemon, perform the HELLO handshake,
-- and store the result in 'pyProc'.
--
-- Sending HELLO immediately registers this process in the daemon's client
-- reference count.  Without it, a window exists between 'connectToDaemon'
-- and the first real MT5 command where the owner sending QUIT could see
-- count == 0 and call _shutdown_daemon, disconnecting all clients.
connectToDaemon :: IO ()
connectToDaemon = do
    h <- connectSocketHandle
    writeIORef pyProc $ Just $ PyProc h h (hClose h)
    -- Bound the HELLO handshake with the same per-cycle deadline used for
    -- ordinary MT5 exchanges.  This path runs inside 'withMT5Lock' as the
    -- reconnect action after a stalled cycle: a daemon that accepts the socket
    -- but never replies would otherwise block 'receive' forever while holding
    -- 'pyProcLock', re-freezing every subsequent MT5 call across all
    -- instruments.  On timeout, drop the fresh socket and surface a recoverable
    -- 'MT5TimeoutException' so upstream retry/backoff proceeds instead of hanging.
    handshake <- timeout mt5CycleTimeoutMicros (send "HELLO" >> void receive)
    case handshake of
        Just _  -> registerReconnectAction connectToDaemon
        Nothing -> do
            hClose h `catch` \(_ :: SomeException) -> return ()
            -- Drop the dead handle from 'pyProc'; leaving it set would let the
            -- next 'send' write to a closed handle ("hPutStr: illegal operation
            -- (handle is closed)"), an error class that never triggers a
            -- reconnect and so loops forever until the watchdog kills the process.
            writeIORef pyProc Nothing
            throwIO (MT5TimeoutException mt5CycleTimeoutMicros)


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
-- Uses a two-phase approach to eliminate startup races:
--
-- 1. Fast path — socket already reachable: connect as client.
-- 2. Acquire the atomic ownership lock (O_CREAT|O_EXCL).
--    Winner: start RPyC server + socket daemon, then connect.
--    Loser: wait up to 30 s for the socket, then connect.
--    Stale lock (dead PID): remove and retry.
--
-- 'mt5DaemonOwner' is set to 'True' only after a successful 'connectToDaemon',
-- so a crash during startup never leaves a false-owner state.
startMT5 :: Config -> IO Config
startMT5 config = do
    avail <- isSocketAvailable
    if avail
        then connectAsClient config
        else acquireAndStart config
  where
    connectAsClient :: Config -> IO Config
    connectAsClient cfg = do
        $(logInfoText) "MT5 daemon already running — connecting as client"
        connectToDaemon
        writeIORef mt5DaemonOwner False
        writeIORef mt5Started True
        return cfg

    acquireAndStart :: Config -> IO Config
    acquireAndStart cfg = do
        acquired <- tryAcquireDaemonLock
        if acquired
            then startDaemonOwner cfg `onException` releaseDaemonLock
            else do
                $(logInfoText) "MT5 daemon ownership held by another process — waiting for socket"
                ready <- waitForSocketTimeout 30
                if ready
                    then connectAsClient cfg
                    else do
                        -- Starter may have failed; stale-lock detection is inside tryAcquireDaemonLock
                        acquired2 <- tryAcquireDaemonLock
                        if acquired2
                            then startDaemonOwner cfg `onException` releaseDaemonLock
                            else error "MT5: could not connect to daemon and could not acquire ownership"

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

        rpycHandle <- startMT5Server config'''
        writeIORef mt5RpycProcess (Just rpycHandle)
        threadDelay (1 * 10^(6 :: Int))

        mRpycPid <- getPid rpycHandle
        startSocketDaemon config''' mRpycPid
        $(logInfoText) "Waiting for MT5 socket daemon to become ready"
        waitForSocket 15

        -- Set owner flag only after a successful connect (fixes the false-owner
        -- bug where a bind failure would still set mt5DaemonOwner=True).
        connectToDaemon
        writeIORef mt5DaemonOwner True
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

        -- | Start the RPyC server as an independent OS process.
        --
        -- Running as a standalone process (not forkIO) means the RPyC server
        -- outlives the owner Haskell process.  The socket daemon sends SIGTERM
        -- to the RPyC process when its last client disconnects.
        startMT5Server :: Config -> IO ProcessHandle
        startMT5Server c = do
            when (winePython c == "") $ error "Need to call installMT5InWine before startMT5Server"
            python <- venvPython c
            devNull <- openFile "/dev/null" ReadWriteMode
            (_, _, _, pHandle) <-
                createProcess (proc python ["-m", "mt5linux", winePython c])
                    { cwd    = Just "."
                    , std_in  = UseHandle devNull
                    , std_out = UseHandle devNull
                    , std_err = UseHandle devNull
                    }
            $(logInfoText) "Successfully started mt5linux RPyC server"
            return pHandle

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


-- | Disconnect from the daemon (and release ownership if this is the owning process).
--
-- All processes send QUIT to gracefully disconnect; the daemon auto-exits when
-- its last client leaves (Python reference counting) and sends SIGTERM to the
-- RPyC server process at that point.
--
-- The owning process additionally releases the inter-process lock so that a
-- new process can become owner if needed.  It does NOT send SHUTDOWN; the
-- daemon and RPyC server stay alive for any remaining connected clients.
--
-- If the daemon process has already exited when the owner stops (e.g. due to
-- an error), the owner terminates the RPyC process defensively to avoid
-- leaving it as an orphan on the same port.
stopMT5 :: IO ()
stopMT5 = do
    mPp <- readIORef pyProc
    case mPp of
        Nothing -> return ()
        Just pp -> do
            void $ try @SomeException (send "QUIT")
            void $ try @SomeException (receive >>= B.putStr)
            pyCleanup pp
            writeIORef pyProc Nothing
    owner <- readIORef mt5DaemonOwner
    when owner $ do
        releaseDaemonLock
        writeIORef mt5DaemonOwner False
        -- If the daemon has already exited, clean up the RPyC server ourselves.
        mDaemon <- readIORef mt5DaemonProcess
        daemonAlive <- case mDaemon of
            Nothing -> return False
            Just ph -> (== Nothing) <$> getProcessExitCode ph
        unless daemonAlive $ do
            mRpyc <- readIORef mt5RpycProcess
            case mRpyc of
                Nothing -> return ()
                Just ph -> do
                    terminateProcess ph
                    void $ try @SomeException $ waitForProcess ph
            writeIORef mt5RpycProcess Nothing
