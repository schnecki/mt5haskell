{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           EasyLogger
import           MT5

main :: IO ()
main = do
  $(initLogger) LogStdOut
  enableMT5Logging LogStdOut
  config <- startMT5 defaultMT5Config
  getLine :: IO String
  stopMT5
