{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | File-based communication with MT5 via JSON files
module MT5.Communication.File
    ( -- * File Operations
      sendRequest
    , receiveResponse
    , initializeFiles
    , getMT5FilePaths
    , getMT5FilesDirDefault
    , getMT5FilesDirCustom
    ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (unless)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BSL
import           Data.Maybe             (fromMaybe)
import           EasyLogger
import           System.Directory       (doesFileExist, getHomeDirectory, renameFile)
import           System.Environment     (lookupEnv)
import           System.FilePath        ((<.>), (</>))
import           System.IO.Error        (catchIOError, isDoesNotExistError)
import           System.Timeout         (timeout)

import           MT5.Communication.Types


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
initializeFiles :: FilePath -> FilePath -> IO ()
initializeFiles reqPath respPath = do
  $(logInfo) $ "Initializing MT5 file communication" ++ ""
  $(logInfo) $ "Request file: " ++ reqPath
  $(logInfo) $ "Response file: " ++ respPath
  
  -- Create files if they don't exist (with empty JSON)
  unlessM (doesFileExist reqPath) $ do
    $(logInfo) $ "Creating request file: " ++ reqPath
    BSL.writeFile reqPath "{}"
  
  unlessM (doesFileExist respPath) $ do
    $(logInfo) $ "Creating response file: " ++ respPath
    BSL.writeFile respPath "{}"
  where
    unlessM :: IO Bool -> IO () -> IO ()
    unlessM condM action = do
      cond <- condM
      unless cond action


-- | Send request to MT5 via file (atomic write)
--
-- Uses temp file + rename pattern for atomic writes on POSIX systems
sendRequest :: FilePath -> Request -> IO ()
sendRequest reqPath req = do
  let tempPath = reqPath <.> "tmp"
  $(logPrintDebug) $ "Writing request to: " ++ reqPath
  $(logPrintDebug) $ "Action: " ++ show (requestAction req)
  
  -- Write to temp file first
  BSL.writeFile tempPath (Aeson.encode req)
  
  -- Atomic rename (POSIX guarantee)
  renameFile tempPath reqPath
  
  $(logPrintDebug) $ "Request written successfully" ++ ""


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
      -- Clear response file to prevent reading stale data
      BSL.writeFile respPath "{}"
      $(logPrintDebug) $ "Response file cleared" ++ ""
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
