{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | File-based communication with MT5 via JSON files
module MT5.Communication.File
    ( -- * File Operations
      sendRequest
    , receiveResponse
    , sendRequestAndReceive
    , initializeFiles
    , getMT5FilePaths
    , getMT5FilesDirDefault
    , getMT5FilesDirCustom
    ) where

import           Control.Concurrent     (threadDelay, MVar, newMVar, withMVar)
import           Control.Exception      (bracket, try)
import           Control.Monad          (unless)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BSL
import           Data.Maybe             (fromMaybe)
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX  (utcTimeToPOSIXSeconds)
import           EasyLogger
import           GHC.IO.Device          (SeekMode(AbsoluteSeek))
import           GHC.IO.Handle          (hClose)
import           System.Directory       (doesFileExist, getHomeDirectory, getModificationTime)
import           System.Environment     (lookupEnv)
import           System.FilePath        ((<.>), (</>))
import           System.IO              (IOMode(..), openFile)
import           System.IO.Error        (catchIOError, isDoesNotExistError)
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Posix.IO        (handleToFd)
import           System.Posix.Types     (Fd)
import           System.Posix.IO.ByteString (setLock, LockRequest(..), waitToSetLock)
import           System.Timeout         (timeout)

import           MT5.Communication.Types


-- | Global lock for file-based communication
--
-- Ensures only one thread can send/receive at a time to prevent file conflicts
fileLock :: MVar ()
fileLock = unsafePerformIO $ newMVar ()
{-# NOINLINE fileLock #-}

-- | Get default MT5 Files directory path (Wine-based)
--
-- Uses WINEPREFIX environment variable if set, otherwise ~/.wine
-- Constructs path: $WINEPREFIX/drive_c/users/$USER/AppData/Roaming/MetaQuotes/Terminal/Common/Files
getMT5FilesDirDefault :: IO FilePath
getMT5FilesDirDefault = do
  winePrefix <- lookupEnv "WINEPREFIX" >>= \case
    Just prefix -> return prefix
    Nothing -> do
      home <- getHomeDirectory
      return (home </> ".wine")
  
  -- Get username from environment
  username <- fromMaybe "schnecki" <$> lookupEnv "USER"
  
  return $ winePrefix 
    </> "drive_c/users"
    </> username
    </> "AppData/Roaming/MetaQuotes/Terminal/Common/Files"


-- | Get custom MT5 Files directory path
getMT5FilesDirCustom :: FilePath -> IO FilePath
getMT5FilesDirCustom customPath = return customPath


-- | Get request and response file paths
--
-- Returns (requestFilePath, responseFilePath)
getMT5FilePaths :: FilePath -> IO (FilePath, FilePath)
getMT5FilePaths baseDir = return
  ( baseDir </> "mt5_api_request.json"
  , baseDir </> "mt5_api_response.json"
  )


-- | Initialize request and response files (create if missing)
--
-- CRITICAL: Files are created once and reused forever.
-- NEVER delete these files during operation!
-- Only logs initialization message if files don't exist yet.
initializeFiles :: FilePath -> FilePath -> IO ()
initializeFiles reqPath respPath = do
  reqExists <- doesFileExist reqPath
  respExists <- doesFileExist respPath
  
  -- Only log if we're actually creating files
  unless (reqExists && respExists) $ do
    $(logInfo) $ "Initializing MT5 file communication" ++ ""
    $(logInfo) $ "Request file: " ++ reqPath
    $(logInfo) $ "Response file: " ++ respPath
  
  -- Create files if they don't exist (with empty JSON)
  unless reqExists $ do
    $(logInfo) $ "Creating request file: " ++ reqPath
    BSL.writeFile reqPath "{}"
  
  unless respExists $ do
    $(logInfo) $ "Creating response file: " ++ respPath
    BSL.writeFile respPath "{}"


-- | Send request to MT5 via file (atomic write)
sendRequest :: FilePath -> Request -> IO ()
sendRequest reqPath req = do
  $(logPrintDebug) $ "Writing request to: " ++ reqPath
  $(logPrintDebug) $ "Action: " ++ show (requestAction req)
  
  -- Simply write the file - the EA will handle any conflicts
  -- We don't need locking since we write atomically and EA polls
  BSL.writeFile reqPath (Aeson.encode req)
  
  $(logPrintDebug) $ "Request written successfully" ++ ""


-- | Send request and receive response atomically (with global lock)
--
-- Uses a global MVar lock to ensure only one request/response cycle happens at a time.
-- This prevents file conflicts when multiple tests or threads try to communicate simultaneously.
--
-- Strategy: Record request time, then only accept responses written AFTER that time.
-- This prevents reading stale responses from previous requests without needing to clear the file.
sendRequestAndReceive :: FilePath -> FilePath -> Request -> Int -> IO (Maybe Response)
sendRequestAndReceive reqPath respPath req timeoutMs =
  withMVar fileLock $ \_ -> do
    -- Record time before sending request
    requestTime <- getCurrentTime
    $(logPrintDebug) $ "Sending request at " ++ show requestTime
    -- Send request (EA will overwrite response file with new response)
    sendRequest reqPath req
    -- Wait for EA to process and write response (only accept responses newer than requestTime)
    receiveResponseAfter timeoutMs respPath requestTime


-- | Receive response from MT5 via file, only accepting responses written after given time
--
-- Polls file every 100ms until timeout expires.
-- Only accepts responses where file modification time is AFTER the request time.
-- This prevents reading stale responses from previous requests.
-- Returns Nothing on timeout, Just Response on success.
receiveResponseAfter :: Int -> FilePath -> UTCTime -> IO (Maybe Response)
receiveResponseAfter timeoutMs respPath afterTime = do
  $(logPrintDebug) $ "Waiting for response after " ++ show afterTime ++ " (timeout: " ++ show timeoutMs ++ "ms)"
  
  result <- timeout (timeoutMs * 1000) $ waitAndReadResponseAfter respPath afterTime
  
  case result of
    Nothing -> do
      $(logError) $ "Response timeout after " ++ show timeoutMs ++ "ms"
      return Nothing
    Just resp -> do
      $(logPrintDebug) $ "Response received: " ++ show (responseSuccess resp)
      return (Just resp)


-- | Wait for response file to be updated AFTER the given time
--
-- Polls file every 100ms. Only reads file if modification time is after afterTime.
-- This ensures we don't read stale responses from previous requests.
waitAndReadResponseAfter :: FilePath -> UTCTime -> IO Response
waitAndReadResponseAfter respPath afterTime = pollFile
  where
    pollFile :: IO Response
    pollFile = do
      exists <- doesFileExist respPath
      if exists
        then tryReadResponse
        else do
          threadDelay 100000  -- Wait 100ms
          pollFile
    
    tryReadResponse :: IO Response
    tryReadResponse = do
      -- Check if file was modified after request time
      modTime <- getModificationTime respPath
      if modTime <= afterTime
        then do
          -- File is stale, wait for EA to update it
          $(logPrintDebug) $ "Response file not yet updated (mod time: " ++ show modTime ++ ")"
          threadDelay 100000
          pollFile
        else do
          -- File was updated after request, try to read it
          readResult <- catchIOError 
            (Just <$> BSL.readFile respPath)
            (\e -> if isDoesNotExistError e
                   then return Nothing
                   else ioError e)
          
          case readResult of
            Nothing -> do
              threadDelay 100000
              pollFile
            Just contents ->
              case Aeson.eitherDecode contents of
                Left err -> do
                  $(logError) $ "Invalid JSON in response file: " ++ err
                  threadDelay 100000
                  pollFile
                Right resp -> return resp


-- | Receive response from MT5 via file (with timeout and polling)
--
-- Polls file every 100ms until timeout expires.
-- Clears response file after successful read to prevent stale data.
-- Returns Nothing on timeout, Just Response on success.
receiveResponse :: Int -> FilePath -> IO (Maybe Response)
receiveResponse timeoutMs respPath = do
  $(logPrintDebug) $ "Waiting for response (timeout: " ++ show timeoutMs ++ "ms)"
  
  result <- timeout (timeoutMs * 1000) $ waitAndReadResponse respPath
  
  case result of
    Nothing -> do
      $(logError) $ "Response timeout after " ++ show timeoutMs ++ "ms"
      return Nothing
    Just resp -> do
      $(logPrintDebug) $ "Response received: " ++ show (responseSuccess resp)
      -- NOTE: We don't clear the response file anymore.
      -- The global lock ensures sequential access, so the next request will overwrite it.
      -- Clearing could cause race conditions where concurrent code reads empty JSON.
      return (Just resp)

-- | Wait for response file to be updated and read it
--
-- Polls file every 100ms. Reads file when it exists and has valid JSON.
waitAndReadResponse :: FilePath -> IO Response
waitAndReadResponse respPath = pollFile
  where
    pollFile :: IO Response
    pollFile = do
      -- Check if file exists and is readable
      exists <- doesFileExist respPath
      if exists
        then tryReadResponse
        else do
          threadDelay 100000  -- Wait 100ms
          pollFile
    
    tryReadResponse :: IO Response
    tryReadResponse = do
      -- Try to read file, handle errors
      readResult <- catchIOError 
        (Just <$> BSL.readFile respPath)
        (\e -> if isDoesNotExistError e
               then return Nothing  -- File doesn't exist
               else ioError e)       -- Other errors are fatal
      
      case readResult of
        Nothing -> do
          threadDelay 100000  -- File deleted between check and read, wait and retry
          pollFile
        Just contents ->
          case Aeson.eitherDecode contents of
            Left err -> do
              $(logError) $ "Invalid JSON in response file: " ++ err
              threadDelay 100000  -- Wait and retry
              pollFile
            Right resp -> return resp
