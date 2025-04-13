{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module MT5.Communication
    ( send
    , receive
    , unpickle'
    ) where

import           Data.Bits              (shiftL, (.|.))
import qualified Data.ByteString        as B
import           Data.Char              (ord)
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
  (PyProc pyIn _ _) <- readIORef pyProc
  $(logPrintDebug) $ "Sending: " ++ cmd
  hPutStrLn pyIn cmd
  hFlush pyIn

-- | Receive output from stdout of python
receive :: IO B.ByteString
receive = do
  (PyProc _ pyOut _) <- readIORef pyProc
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
  fromVal = fmap T.unpack . fromVal


-- errorUnpickle :: String -> B.ByteString -> a
-- errorUnpickle tp x = error $ "Could not parse `" ++ tp ++ "` from: " ++ show x

-- unpickle' :: FromValue a => String -> B.ByteString -> a
-- unpickle' tp bs = tryUnpickle' (errorUnpickle tp) bs

-- tryUnpickle' :: FromValue a => (B.ByteString -> a) -> B.ByteString -> a
-- tryUnpickle' alt bs =
--   case unpickle bs of
--     Left str -> error str
--     Right x  -> fromMaybe (alt bs) (fromVal x)

-- instance {-# OVERLAPS #-} FromValue String where
--     fromVal = fmap T.unpack . fromVal


-- rcvString :: IO String
-- rcvString = unpickle' "String" <$> receive

-- rcvIntList :: IO [Int]
-- rcvIntList = unpickle' "[Int]" <$> receive

-- rcvInt :: IO Int
-- rcvInt = unpickle' "Int" <$> receive


-- rcvDblList :: IO [Double]
-- rcvDblList = tryUnpickle' (pure . tryUnpickle' (errorUnpickle "[Double]")) <$> receive


-- rcvDbl :: IO Double
-- rcvDbl = unpickle' "Double" <$> receive


-- -- | Set the environment
-- setEnv :: IO ()
-- setEnv = sendCmd $ EnvSet envName (T.unpack . T.replace "'" "\"" . T.pack $ envArgs)

-- getEnv :: IO String
-- getEnv = do
--   sendCmd EnvGet
--   rcvString

-- -- | Send exit command and get rid of handles
-- quit :: IO ()
-- quit = do
--   sendCmd Quit
--   rcvString >>= putStrLn
--   (PyProc inp outp phandle) <- readIORef py
--   cleanupProcess (Just inp, Just outp, Nothing, phandle)
--   exitSuccess
