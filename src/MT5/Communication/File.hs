{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | File-based communication with MT5 via JSON files
module MT5.Communication.File
    ( -- * Per-request file protocol
      --
      -- $protocol
      sendRequestAndReceivePerRequest
    , resetMT5Files
    , getMT5FilesDirDefault
    , getMT5FilesDirCustom
    , perRequestFileNames
    , requestFilePrefix
    , responseFilePrefix
    ) where

import           Control.Concurrent     (threadDelay)
import           Control.Exception      (IOException, catch, finally)
import           Control.Monad          (unless)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BSL
import           Data.IORef             (IORef, atomicModifyIORef', newIORef)
import           Data.List              (isPrefixOf)
import           Data.Maybe             (fromMaybe)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Clock.POSIX  (utcTimeToPOSIXSeconds)
import           EasyLogger
import           System.Directory       (doesFileExist, getHomeDirectory, listDirectory,
                                          removeFile, renameFile)
import           System.Environment     (lookupEnv)
import           System.FilePath        ((<.>), (</>))
import           System.IO.Error        (catchIOError, isDoesNotExistError)
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Posix.Process   (getProcessID)
import           System.Timeout         (timeout)

import           MT5.Communication.Types


-- $protocol
--
-- Every request gets its own pair of files, named by an id unique to the
-- caller: @mt5_api_req_\<id\>.json@ carries the request, and the EA answers in
-- @mt5_api_resp_\<id\>.json@. Both are deleted once the reply has been read.
--
-- The id is what makes this safe. An earlier design shared one fixed
-- request\/response file pair between every caller and accepted a reply for
-- being newer than the request that was just sent. That test cannot tell one
-- caller's reply from another's, so two processes talking to the same
-- terminal — a live trader and a bulk data collector, say — could read each
-- other's payloads, and a candle array fetched for one symbol could be stored
-- as another symbol's history. Naming the reply after the request it answers
-- removes the ambiguity at its source, so no locking is needed between
-- processes and no reply is ever accepted on the strength of its timestamp.


-- | Filename prefix the EA scans for when picking up per-request files.
--   Must match @RequestPrefix@ in MT5RestAPIBridge.mq5.
requestFilePrefix :: String
requestFilePrefix = "mt5_api_req_"


-- | Filename prefix the EA writes replies under. Must match @ResponsePrefix@
--   in MT5RestAPIBridge.mq5.
responseFilePrefix :: String
responseFilePrefix = "mt5_api_resp_"


-- | Counter making request ids unique within a process, since two requests
--   issued inside the same clock tick would otherwise collide.
perRequestCounter :: IORef Integer
perRequestCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE perRequestCounter #-}


-- | A request id unique across processes and threads: the OS process id pins
--   it to this process, the counter separates concurrent requests inside it,
--   and the clock separates successive runs of the same pid.
newRequestId :: IO String
newRequestId = do
  pid <- getProcessID
  n <- atomicModifyIORef' perRequestCounter (\k -> (k + 1, k + 1))
  now <- utcTimeToPOSIXSeconds <$> getCurrentTime
  return $ show (fromEnum pid) ++ "_" ++ show (round (now * 1000) :: Integer) ++ "_" ++ show n


-- | Request and response file names for one request id. The names carry the
--   id so a response can only ever be matched to the request that asked for
--   it, which is what makes the per-request protocol safe without locking.
perRequestFileNames :: String -> (FilePath, FilePath)
perRequestFileNames reqId =
  (requestFilePrefix ++ reqId ++ ".json", responseFilePrefix ++ reqId ++ ".json")


-- | Send one request under its own file name and wait for the reply written
--   under the matching name.
--
--   This is the safe alternative to 'sendRequestAndReceive'. That function
--   shares one fixed request\/response file pair between every caller, so a
--   reply is accepted merely for being recent — two processes racing can read
--   each other's payload, and a candle array for one symbol can be parsed as
--   the answer to a different symbol's request. Here the id in the file name
--   ties a reply to its request, so a foreign reply is never even looked at.
--
--   The request is written to a temporary name and renamed into place, so the
--   EA never observes a half-written request. Both files are removed once the
--   reply has been read, and the request file is removed on timeout too, so a
--   failed call leaves nothing behind for the EA to pick up later.
sendRequestAndReceivePerRequest :: FilePath -> Request -> Int -> IO (Maybe Response)
sendRequestAndReceivePerRequest baseDir req timeoutMs = do
  reqId <- newRequestId
  let (reqName, respName) = perRequestFileNames reqId
      reqPath = baseDir </> reqName
      respPath = baseDir </> respName
      tmpPath = reqPath <.> "tmp"
  $(logPrintDebug) $ "Per-request send: " ++ reqName ++ " action " ++ show (requestAction req)
  BSL.writeFile tmpPath (Aeson.encode req)
  renameFile tmpPath reqPath
  result <- timeout (timeoutMs * 1000) (waitForPerRequestResponse respPath)
    `finally` removeFileIfExists tmpPath
  case result of
    Just resp -> do
      removeFileIfExists respPath
      $(logPrintDebug) $ "Per-request reply read and removed: " ++ respName
      return (Just resp)
    Nothing -> do
      -- The EA may never have picked the request up, or may still be working on
      -- it. Drop both sides: a late reply to an abandoned request must not be
      -- left lying around, and the request must not be executed after we gave up.
      removeFileIfExists reqPath
      removeFileIfExists respPath
      $(logError) $ "Per-request timeout after " ++ show timeoutMs ++ "ms (" ++ reqName ++ ")"
      return Nothing


-- | Poll for this request's own response file. No modification-time test is
--   needed: the file name is unique to the request, so its mere existence
--   means the reply belongs to this caller.
waitForPerRequestResponse :: FilePath -> IO Response
waitForPerRequestResponse respPath = poll
  where
    poll = do
      exists <- doesFileExist respPath
      if not exists
        then threadDelay 20000 >> poll
        else do
          readResult <- catchIOError (Just <$> BSL.readFile respPath)
            (\e -> if isDoesNotExistError e then return Nothing else ioError e)
          case readResult >>= Aeson.decode of
            Just resp -> return resp
            -- An unparsable read here is almost always a partially written
            -- file caught mid-write; retry rather than fail the request.
            Nothing -> threadDelay 20000 >> poll


-- | Remove a file, ignoring the case where it is already gone.
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path =
  removeFile path `catch` \(_ :: IOException) -> return ()


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


-- | Delete every request and response file left in the exchange directory.
--
--   Under the per-request protocol a completed call removes its own files, so
--   anything still present belongs to a call that died or to a terminal that
--   was restarted mid-exchange. Sweeping them keeps the EA from executing a
--   request nobody is waiting for any more. Safe to call at startup and after
--   a detected communication failure; it never touches unrelated files,
--   because it matches only the two bridge prefixes.
resetMT5Files :: FilePath -> IO ()
resetMT5Files baseDir = do
  entries <- listDirectory baseDir `catch` \(_ :: IOException) -> return []
  let stale = filter isBridgeFile entries
  unless (null stale) $
    $(logInfo) $ "Sweeping " ++ show (length stale) ++ " stale MT5 bridge file(s) in " ++ baseDir
  mapM_ (removeFileIfExists . (baseDir </>)) stale
  where
    isBridgeFile name =
      any (`isPrefixOf` name) [requestFilePrefix, responseFilePrefix]


