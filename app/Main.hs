{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           Control.Monad
import           EasyLogger
import           MT5
import           System.IO

main :: IO ()
main = do
  $(initLogger) LogStdOut
  enableMT5Logging LogStdOut
  config <- startMT5 defaultMT5Config
  -- putStr "Press enter to login" >> hFlush stdout
  -- _ <- getLine :: IO String
  res <- initialize
  putStrLn $ "Initialize result: " <> show res
  -- -- res <- login (MT5Login "" "") -- does not work
  -- putStrLn $ "Login result: " <> show res
  accountInfo >>= print
  putStrLn $ "Account Info done"
  ordersGet Nothing Nothing >>= print
  putStrLn $ "OrdersGet done"
  positionsGet >>= print
  putStr "Press enter to exit" >> hFlush stdout
  -- symbolSelect "EURJPY.pro" >>= print
  -- symbolInfo "EURJPY.pro" >>= print
  -- recurse ""
  -- stopMT5
  -- putStrLn ""


  where recurse x = do
          unless (x == "q") $ do
            symbolsGet "US500*" >>= print
            symbolInfo "US500.pro"
            res <- orderSend
              $ MqlTradeRequest
                  TRADE_ACTION_PENDING
                  0
                  0
                  "EURUSD.pro"
                  0.10
                  1.1008
                  1.1008
                  1.095
                  1.12
                  3
                  ORDER_TYPE_BUY
                  ORDER_FILLING_FOK
                  ORDER_TIME_GTC
                  0
                  "MT5 test order"
                  0
                  0
            print res
            getLine >>= recurse

