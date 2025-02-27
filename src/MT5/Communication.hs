{-# LANGUAGE TemplateHaskell #-}
module MT5.Communication
    ( send
    , receive
    ) where

import qualified Data.ByteString as B
import           Data.IORef
import           EasyLogger
import           System.IO

import           MT5.PyProc

-- | Send a command + arguments to python stdin
send :: String -> IO ()
send cmd = do
  (PyProc pyIn _ _) <- readIORef pyProc
  $(logPrintInfo) $ "Sending: " ++ cmd
  hPutStrLn pyIn cmd
  hFlush pyIn


-- | Receive output from stdout of python
receive :: IO B.ByteString
receive = do
  (PyProc _ pyOut _) <- readIORef pyProc
  line <- B.hGetLine pyOut
  putStr "Received: " >> print line
  -- print str
  -- if T.isInfixOf "info" (T.toLower . T.pack $ str)
  --   then putStrLn (">>>> " ++ str) >> hFlush stdout >> receive
  --   else return str
  return line

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
