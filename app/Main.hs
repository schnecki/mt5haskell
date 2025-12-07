{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           Control.Exception
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad
import qualified Data.Text         as T
import           Data.Time
import           EasyLogger
import           MT5
import           MT5.Data.Candle   (MT5Candle (..), MT5CandleData (..))
import           MT5.Data.DecimalNumber (DecimalNumber (..))

-- | Example: Get candle data for a specific time range
-- Demonstrates getCandleDataRange which maps to COPY_RATES_RANGE
testCandleDataRange :: IO ()
testCandleDataRange = do
  putStrLn "\n=== Testing getCandleDataRange (COPY_RATES_RANGE) ==="
  putStrLn "Getting EURUSD H4 candles from 2025-04-04 00:00:00 to 2025-04-04 18:00:00"

  from <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2025-04-04 00:00:00"
  to <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2025-04-04 18:00:00"

  result <- getCandleDataRange "EURUSD" H4 from to
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right candleData -> do
      putStrLn $ "Success! Retrieved " ++ show (length (mt5Candles candleData)) ++ " candles for " ++ mt5Symbol candleData
      -- Display first candle with all fields including spread and real volume
      case mt5Candles candleData of
        (firstCandle:_) -> do
          putStrLn "First candle details:"
          putStrLn $ "  Time: " ++ show (mt5CandleTime firstCandle)
          putStrLn $ "  OHLC: " ++ show (mt5CandleOpen firstCandle, mt5CandleHigh firstCandle, mt5CandleLow firstCandle, mt5CandleClose firstCandle)
          putStrLn $ "  Tick Volume: " ++ show (mt5CandleVolume firstCandle)
          putStrLn $ "  Spread: " ++ show (mt5CandleSpread firstCandle) ++ " points"
          putStrLn $ "  Real Volume: " ++ show (mt5CandleRealVolume firstCandle)
        [] -> putStrLn "No candles retrieved"

-- | Example: Get a specific count of candles from a start date
-- Demonstrates getCandleDataFrom which maps to COPY_RATES_FROM
testCandleDataFrom :: IO ()
testCandleDataFrom = do
  putStrLn "\n=== Testing getCandleDataFrom (COPY_RATES_FROM) ==="
  putStrLn "Getting last 10 EURUSD M15 candles from 2025-04-04 12:00:00"

  fromTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2025-04-04 12:00:00"

  result <- getCandleDataFrom "EURUSD" M15 fromTime 10
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right candleData -> do
      putStrLn $ "Success! Retrieved " ++ show (length (mt5Candles candleData)) ++ " candles for " ++ mt5Symbol candleData
      -- Display summary of all candles
      mapM_ displayCandleSummary (zip [1::Int ..] (mt5Candles candleData))
  where
    displayCandleSummary (idx, candle) = do
      putStrLn $ "Candle " ++ show idx ++ ": " ++
                 show (mt5CandleTime candle) ++ " | " ++
                 "Close: " ++ show (mt5CandleClose candle) ++ " | " ++
                 "Spread: " ++ show (mt5CandleSpread candle) ++ "pts"

-- | Example: Get recent candles using position-based indexing
-- Demonstrates getCandleDataRecent which maps to COPY_RATES_FROM_POS
testCandleDataRecent :: IO ()
testCandleDataRecent = do
  putStrLn "\n=== Testing getCandleDataRecent (COPY_RATES_FROM_POS) ==="
  putStrLn "Getting 5 most recent EURUSD M5 candles"

  result <- getCandleDataRecent "EURUSD" M5 5
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right candleData -> do
      putStrLn $ "Success! Retrieved " ++ show (length (mt5Candles candleData)) ++ " recent candles for " ++ mt5Symbol candleData
      -- Display detailed information including new fields
      putStrLn "Recent candles (newest first):"
      mapM_ displayDetailedCandle (reverse $ zip [1::Int ..] (mt5Candles candleData))
  where
    displayDetailedCandle (idx, candle) = do
      putStrLn $ "  " ++ show idx ++ ". " ++ show (mt5CandleTime candle)
      putStrLn $ "     OHLC: " ++ formatOHLC candle
      putStrLn $ "     Volume: " ++ show (mt5CandleVolume candle) ++ " ticks, Real: " ++ show (mt5CandleRealVolume candle)
      putStrLn $ "     Spread: " ++ show (mt5CandleSpread candle) ++ " points"

    formatOHLC candle = show (mt5CandleOpen candle) ++ "/" ++
                       show (mt5CandleHigh candle) ++ "/" ++
                       show (mt5CandleLow candle) ++ "/" ++
                       show (mt5CandleClose candle)

-- | Test all candle data retrieval methods
testAllCandleAPIs :: IO ()
testAllCandleAPIs = do
  putStrLn "=== Comprehensive Candle Data API Testing ==="
  putStrLn "Testing all three MT5 copy_rates_* API methods with new spread and real volume fields"

  testCandleDataRange
  testCandleDataFrom
  testCandleDataRecent

  putStrLn "\n=== All Candle API Tests Complete ==="

main :: IO ()
main = do
  $(initLogger) LogStdOut
  enableMT5Logging LogStdOut
  mainAction `finally` flushLoggers
  where
    mainAction = do


      _ <- startMT5 defaultMT5Config {mt5linuxLocalPath = Just "../mt5linux/"}

      putStrLn "=== MT5 Haskell API Demo ==="
      putStrLn "Initializing MT5 connection..."

      res <- initialize
      putStrLn $ "Initialize result: " ++ show res
      putStrLn "Account Info:"
      runExceptT accountInfo >>= print
      -- putStrLn "Symbols:"
      -- symbolsGet Nothing >>= print

      case res of
        Right () -> do
          putStrLn "MT5 initialized successfully!"
          testAllCandleAPIs
        Left err -> do
          putStrLn $ "Failed to initialize MT5: " ++ err

      putStrLn "Order send:"
      res <- runExceptT $ orderSend $ MqlTradeRequest
            { trReqAction = TRADE_ACTION_DEAL
            , trReqMagic = 12345
            , trReqOrder = 0
            , trReqSymbol = "EURUSD.PRO"
            , trReqVolume = 0.01  -- (must be positive)
            , trReqPrice = DecimalNumber Nothing 0.0
            , trReqStoplimit = DecimalNumber Nothing 0.0
            , trReqSl = DecimalNumber Nothing 1.18
            , trReqTp = DecimalNumber Nothing 0.0
            , trReqDeviation = 20
            , trReqType = ORDER_TYPE_SELL
            , trReqTypeFilling = ORDER_FILLING_IOC
            , trReqTypeTime = ORDER_TIME_GTC
            , trReqExpiration = 0
            , trReqComment = "aral-trader market order"
            , trReqPosition = 0
            , trReqPositionBy = 0
            }

      print res

      putStrLn "\nPress enter to exit..."
      void (getLine :: IO String)
