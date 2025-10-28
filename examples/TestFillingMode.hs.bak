{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bits          ((.&.))
import           MT5
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Text.Printf        (printf)

main :: IO ()
main = do
  args <- getArgs
  let symbol = case args of
        []    -> "EURUSD.pro"  -- Default symbol
        (s:_) -> s

  putStrLn $ "\n=== MT5 Symbol Filling Mode Test ==="
  putStrLn $ "Symbol: " ++ symbol
  putStrLn ""

  -- Initialize MT5
  putStrLn "Initializing MT5..."
  startMT5 defaultMT5Config

  -- Get symbol info
  putStrLn $ "Querying symbol info for " ++ symbol ++ "..."
  symInfo <- symbolInfo symbol

  let fillingMode = symInfoFillingMode symInfo

  putStrLn $ "\n✅ Symbol found: " ++ symbol
  putStrLn $ "\nFilling Mode: " ++ show fillingMode
  putStrLn ""

  -- Decode bitmask
  putStrLn "=== Supported Filling Types ==="
  putStrLn ""

  let fokSupported    = (fillingMode .&. 1) /= 0  -- Bit 0
      iocSupported    = (fillingMode .&. 2) /= 0  -- Bit 1
      returnSupported = (fillingMode .&. 4) /= 0  -- Bit 2

  putStrLn $ if fokSupported
             then "✅ ORDER_FILLING_FOK (Fill or Kill) - SUPPORTED"
             else "❌ ORDER_FILLING_FOK (Fill or Kill) - NOT supported"

  putStrLn $ if iocSupported
             then "✅ ORDER_FILLING_IOC (Immediate or Cancel) - SUPPORTED"
             else "❌ ORDER_FILLING_IOC (Immediate or Cancel) - NOT supported"

  putStrLn $ if returnSupported
             then "✅ ORDER_FILLING_RETURN (Return) - SUPPORTED"
             else "❌ ORDER_FILLING_RETURN (Return) - NOT supported"

  putStrLn ""
  putStrLn "=== Analysis ==="
  putStrLn ""

  -- Analyze for position closure
  if not fokSupported && not iocSupported && not returnSupported then do
    putStrLn "⚠️  WARNING: No filling modes supported!"
    putStrLn "This symbol might not allow trading at all."
  else if fokSupported && not iocSupported then do
    putStrLn "🎯 DIAGNOSIS: Only FOK is supported!"
    putStrLn ""
    putStrLn "This confirms the hypothesis:"
    putStrLn "  - Aral-trader uses ORDER_FILLING_IOC"
    putStrLn "  - Broker only accepts ORDER_FILLING_FOK"
    putStrLn "  - This causes error 10013 (Invalid request)"
    putStrLn ""
    putStrLn "✅ SOLUTION: Change aral-trader to use ORDER_FILLING_FOK"
    putStrLn ""
    putStrLn "In aral-trader order request construction:"
    putStrLn "  BEFORE: trReqTypeFilling = ORDER_FILLING_IOC"
    putStrLn "  AFTER:  trReqTypeFilling = ORDER_FILLING_FOK"
    putStrLn ""
    putStrLn "OR use library's positionClose function which already uses FOK."
  else if fokSupported && iocSupported then do
    putStrLn "✅ Both FOK and IOC are supported"
    putStrLn ""
    putStrLn "This means type_filling is NOT the issue."
    putStrLn "The error 10013 is likely caused by:"
    putStrLn "  - Price parameter (price=0 not accepted)"
    putStrLn "  - Magic number mismatch"
    putStrLn "  - Other broker restrictions"
    putStrLn ""
    putStrLn "Next steps:"
    putStrLn "  1. Recompile EA with POSITION_PRICE_CURRENT enhancement"
    putStrLn "  2. Test position closure again"
    putStrLn "  3. Check EA log for actual price used"
  else do
    putStrLn $ "Filling modes: FOK=" ++ show fokSupported ++
               " IOC=" ++ show iocSupported ++
               " RETURN=" ++ show returnSupported
    putStrLn ""
    putStrLn "Use the supported filling type in your orders."

  putStrLn ""
  putStrLn "=== Additional Symbol Info ==="
  printf "Bid: %.5f\n" (symInfoBid symInfo)
  printf "Ask: %.5f\n" (symInfoAsk symInfo)
  printf "Digits: %d\n" (symInfoDigits symInfo)
  printf "Spread: %d points\n" (symInfoSpread symInfo)
  putStrLn $ "Trade Mode: " ++ show (symInfoTradeMode symInfo)
  putStrLn ""
