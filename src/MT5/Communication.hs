{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TemplateHaskell   #-}
module MT5.Communication
    ( send
    , receive
    , unpickle'
    , pythonCode
    ) where

import           Data.Bits              (shiftL, (.|.))
import qualified Data.ByteString        as B
import           Data.Char              (ord)
import           Data.FileEmbed         (embedFileRelative)
import           Data.IORef
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Data.Word
import           EasyLogger
import           Language.Python.Pickle hiding (unpickle')
import           System.IO

import           MT5.PyProc

import           Debug.Trace


-- | Send a command + arguments to python stdin
send :: String -> IO ()
send cmd = do
  (PyProc pyIn _ _ _) <- readIORef pyProc
  $(logPrintDebug) $ "Sending: " ++ cmd
  hPutStrLn pyIn cmd
  hFlush pyIn

-- | Receive output from stdout of python
receive :: IO B.ByteString
receive = do
  (PyProc _ pyOut _ _) <- readIORef pyProc
  readNextObject pyOut
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
pythonCode = $(embedFileRelative "main.py")
