{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TemplateHaskell   #-}
module MT5.Communication.Python
    ( send
    , receive
    , unpickle'
    , pythonCode
    , withMT5Lock
    , registerReconnectAction
    , MT5TimeoutException (..)
    ) where

import           MT5.Embedded.MainPy    (mainPyContent)
import           Data.Bits              (shiftL, (.|.))
import qualified Data.ByteString        as B
import           Data.IORef
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as Encoding
import           Data.Word
import           EasyLogger
import           Language.Python.Pickle hiding (unpickle')
import           System.IO
import           System.IO.Error        (mkIOError, resourceVanishedErrorType)

import           MT5.Communication.PyProc (MT5TimeoutException (..), PyProc (..),
                                           pyProc, registerReconnectAction,
                                           withMT5Lock)


-- | Signal that no daemon connection is currently established.
--
-- Thrown when 'pyProc' is 'Nothing' — e.g. after a reconnect handshake timed
-- out and dropped the dead connection.  A @ResourceVanished@ 'IOError' (not a
-- bare 'error') so 'withMT5Lock' classifies it as recoverable and runs the
-- reconnect action, which re-establishes 'pyProc'; a raw 'error' would escape
-- that path and loop "MT5 not started" until the watchdog restarts the process.
notStartedError :: IO a
notStartedError =
  ioError $ mkIOError resourceVanishedErrorType
    "MT5 daemon connection not established (reconnect required)" Nothing Nothing

-- | Send a command or argument line to the MT5 daemon.
send :: String -> IO ()
send cmd = do
    mPyProc <- readIORef pyProc
    case mPyProc of
        Nothing -> notStartedError
        Just pp -> do
            $(logPrintDebug) $ "Sending: " ++ cmd
            hPutStrLn (pyOut pp) cmd
            hFlush (pyOut pp)

-- | Receive one pickle-encoded response from the MT5 daemon.
receive :: IO B.ByteString
receive = do
    mPyProc <- readIORef pyProc
    case mPyProc of
        Nothing -> notStartedError
        Just pp -> readNextObject (pyIn pp)
  where
    readNextObject :: Handle -> IO B.ByteString
    readNextObject h = do
        lengthBytes <- B.hGet h 4
        if B.length lengthBytes /= 4
            then error "MT5 daemon closed the connection unexpectedly"
            else do
                let len = bytesToWord32 lengthBytes
                B.hGet h (fromIntegral len)

    bytesToWord32 :: B.ByteString -> Word32
    bytesToWord32 bs =
        let [b1, b2, b3, b4] = B.unpack bs
         in (fromIntegral b1 `shiftL` 24)
              .|. (fromIntegral b2 `shiftL` 16)
              .|. (fromIntegral b3 `shiftL` 8)
              .|. fromIntegral b4


unpickle' :: (Show a, FromValue a) => String -> B.ByteString -> a
unpickle' tp bs = tryUnpickle' errorUnpickle bs
  where
    errorUnpickle :: B.ByteString -> a
    errorUnpickle x = error $ "Could not parse `" ++ tp ++ "` from: " ++ show x


tryUnpickle' :: FromValue a => (B.ByteString -> a) -> B.ByteString -> a
tryUnpickle' alt bs =
    case unpickle bs of
        Left str -> error str
        Right x  -> fromMaybe (alt bs) (fromVal x)

instance {-# OVERLAPS #-} FromValue String where
    fromVal :: Value -> Maybe String
    fromVal = fmap T.unpack . fromVal


-- | Embedded Python server source with DEBUG forced to False.
pythonCode :: B.ByteString
pythonCode = disableDebugging (str2Bs mainPyContent)
  where
    disableDebugging inp =
        let (bef, after) = B.breakSubstring (str2Bs "DEBUG=True") inp
         in if B.null after
                then inp  -- "DEBUG=True" not present; nothing to replace
                else bef `B.append` str2Bs "DEBUG=False"
                       `B.append` B.drop (B.length (str2Bs "DEBUG=True")) after
    str2Bs :: String -> B.ByteString
    str2Bs = Encoding.encodeUtf8 . T.pack
