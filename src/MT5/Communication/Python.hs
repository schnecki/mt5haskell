{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TemplateHaskell   #-}
module MT5.Communication.Python
    ( send
    , receive
    , unpickle'
    , pythonCode
    ) where

import           MT5.Embedded.MainPy    (mainPyContent)
import           Data.Bits              (shiftL, (.|.))
import qualified Data.ByteString        as B
import           Data.Char              (ord)
import           Data.IORef
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as Encoding
import           Data.Word
import           EasyLogger
import           Language.Python.Pickle hiding (unpickle')
import           System.IO

import           MT5.Communication.PyProc

import           Debug.Trace


-- | Send a command + arguments to python stdin
send :: String -> IO ()
send cmd = do
  mPyProc <- readIORef pyProc
  case mPyProc of
    Nothing -> error "PyProc object was not set. You need to call startMT5!"
    Just (PyProc pyIn _ _ _) -> do
      $(logPrintDebug) $ "Sending: " ++ cmd
      hPutStrLn pyIn cmd
      hFlush pyIn

-- | Receive output from stdout of python
receive :: IO B.ByteString
receive = do
  mPyProc <- readIORef pyProc
  case mPyProc of
    Nothing -> error "PyProc object was not set. You need to call startMT5!"
    Just (PyProc _ pyOut _ _) -> readNextObject pyOut
  where
    readNextObject :: Handle -> IO B.ByteString
    readNextObject h = do
      lengthBytes <- B.hGet h 4 -- read 4-byte length header
      if B.length lengthBytes /= 4
        then error "Expected bytestring from python" -- end-of-stream or error reading header
        else do
          let len = bytesToWord32 lengthBytes
          B.hGet h (fromIntegral len)
    -- | Converts a 4-byte ByteString (big-endian) to a Word32.
    bytesToWord32 :: B.ByteString -> Word32
    bytesToWord32 bs =
      let [b1, b2, b3, b4] = B.unpack bs
       in (fromIntegral b1 `shiftL` 24)
            .|. (fromIntegral b2 `shiftL` 16)
            .|. (fromIntegral b3 `shiftL` 8)
            .|. fromIntegral b4


unpickle' :: (Show a, FromValue a) => String -> B.ByteString -> a
unpickle' tp bs =
  let res = tryUnpickle' errorUnpickle bs
   -- in trace ("res: " ++ show res) res
  in res
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


pythonCode :: B.ByteString
pythonCode = disableDebugging $ str2Bs mainPyContent
  where disableDebugging inp =
          let (bef, after) = B.breakSubstring (str2Bs "DEBUG=True") inp
          in bef `B.append` str2Bs "DEBUG=False" `B.append` B.drop (B.length (str2Bs "DEBUG=True")) after
        str2Bs :: String -> B.ByteString
        str2Bs = Encoding.encodeUtf8 . T.pack
