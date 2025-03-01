{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           EasyLogger
import           MT5
import           System.IO

main :: IO ()
main = do
  $(initLogger) LogStdOut
  enableMT5Logging LogStdOut
  config <- startMT5 defaultMT5Config
  putStr "Press enter to login" >> hFlush stdout
  getLine :: IO String
  res <- initialize
  putStrLn $ "Initialize result: " <> show res
  -- res <- login (MT5Login "" "") -- does not work
  -- putStrLn $ "Login result: " <> show res
  accountInfo >>= putStrLn . show
  putStr "Press enter to exit" >> hFlush stdout
  getLine :: IO String
  stopMT5
