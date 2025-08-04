module MT5.Data.CandleSpec (candleTests) where

import Data.Time (parseTimeOrError)
import Data.Time.Format (defaultTimeLocale)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import MT5.Data.Candle

candleTests :: TestTree
candleTests = testGroup "Candle Tests"
  [ candleUnitTests
  , candlePropertyTests
  ]

candleUnitTests :: TestTree
candleUnitTests = testGroup "Candle Unit Tests"
  [ testCandleConstruction
  , testFieldAccess
  ]

testCandleConstruction :: TestTree
testCandleConstruction = testCase "Candle construction with all fields" $ do
  let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 12:00:00"
      candle = MT5Candle time 1.0850 1.0855 1.0848 1.0852 1500 3 250.5
  mt5CandleSpread candle @?= 3
  mt5CandleRealVolume candle @?= 250.5
  mt5CandleOpen candle @?= 1.0850
  mt5CandleVolume candle @?= 1500

testFieldAccess :: TestTree
testFieldAccess = testCase "Field access and calculations" $ do
  let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 12:00:00"
      candle = MT5Candle time 1.0850 1.0855 1.0848 1.0852 1500 3 250.5
      range = mt5CandleHigh candle - mt5CandleLow candle
  -- Use tolerance for floating point comparison
  abs (range - 7.0e-4) < 1e-10 @?= True

candlePropertyTests :: TestTree
candlePropertyTests = testGroup "Candle Property Tests"
  [ testSpreadNonNegative
  , testRealVolumeNonNegative
  , testVolumeNonNegative
  ]

testSpreadNonNegative :: TestTree
testSpreadNonNegative = testProperty "Spread should be non-negative" $
  \open high low close volume spread realVol ->
    let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 12:00:00"
        candle = MT5Candle time open high low close volume (abs spread) (abs realVol)
    in mt5CandleSpread candle >= 0

testRealVolumeNonNegative :: TestTree
testRealVolumeNonNegative = testProperty "Real volume should be non-negative" $
  \open high low close volume spread realVol ->
    let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 12:00:00"
        candle = MT5Candle time open high low close volume (abs spread) (abs realVol)
    in mt5CandleRealVolume candle >= 0

testVolumeNonNegative :: TestTree
testVolumeNonNegative = testProperty "Tick volume should be non-negative" $
  \open high low close volume spread realVol ->
    let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 12:00:00"
        candle = MT5Candle time open high low close (abs volume) (abs spread) (abs realVol)
    in mt5CandleVolume candle >= 0
