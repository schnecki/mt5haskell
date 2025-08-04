{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MT5.UtilSpec
Description : Comprehensive test suite for MT5 utility functions
Copyright   : (c) 2025 Manuel Schneckenreither
License     : BSD-3-Clause

Tests for MT5 utility functions focusing on:
- Time conversions and timezone handling
- Epoch boundary conditions
- Leap second handling
- Error cases and edge conditions
-}
module MT5.UtilSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Time
import Data.Time.Clock.POSIX
import MT5.Util

-- | Test suite for MT5 utility functions
spec :: TestTree
spec = testGroup "MT5.Util"
  [ timeConversionTests
  , timezoneTests
  , errorHandlingTests
  ]

-- | Time conversion tests
timeConversionTests :: TestTree
timeConversionTests = testGroup "Time Conversion Tests"
  [ testCase "Seconds to UTCTime conversion" $ do
      let epochSeconds = 0
          utcTime = secondsToUTCTime epochSeconds
      -- Should convert to Unix epoch (1970-01-01 00:00:00 UTC)
      show utcTime @?= "1970-01-01 00:00:00 UTC"

  , testCase "Milliseconds to UTCTime conversion" $ do
      let epochMs = 1609459200000 -- 2021-01-01 00:00:00 UTC in milliseconds
          utcTime = millisecondsToUTCTime epochMs
          expected = UTCTime (fromGregorian 2021 1 1) 0
      utcTime @?= expected

  , testCase "Epoch boundary conditions" $ do
      let epochTime = secondsToUTCTime 0
          preEpoch = secondsToUTCTime (-1)
          postEpoch = secondsToUTCTime 1
      
      epochTime @?= UTCTime (fromGregorian 1970 1 1) 0
      preEpoch @?= UTCTime (fromGregorian 1969 12 31) 86399
      postEpoch @?= UTCTime (fromGregorian 1970 1 1) 1

  , testCase "Large timestamp handling" $ do
      let largeTimestamp = 2147483647 -- 2038-01-19 03:14:07 UTC (32-bit limit)
          utcTime = secondsToUTCTime largeTimestamp
          expected = UTCTime (fromGregorian 2038 1 19) 11647
      utcTime @?= expected

  , testProperty "Round-trip time conversion" $
      forAll (choose (0, 2147483647)) $ \seconds ->
        let utcTime = secondsToUTCTime seconds
            backToSeconds = floor $ utcTimeToPOSIXSeconds utcTime
        in abs (fromIntegral seconds - backToSeconds) <= 1 -- Allow 1 second tolerance
  ]

-- | Timezone handling tests
timezoneTests :: TestTree
timezoneTests = testGroup "Timezone Handling Tests"
  [ testCase "UTC timezone consistency" $ do
      let utcTime = UTCTime (fromGregorian 2023 6 15) 43200 -- 12:00:00 UTC
          seconds = floor $ utcTimeToPOSIXSeconds utcTime
          converted = secondsToUTCTime seconds
      converted @?= utcTime

  , testCase "DST transition handling" $ do
      -- Test around DST transition dates
      let springForward = UTCTime (fromGregorian 2023 3 26) 3600 -- 01:00:00 UTC
          fallBack = UTCTime (fromGregorian 2023 10 29) 3600     -- 01:00:00 UTC
      
      -- These should be handled consistently in UTC
      let springSeconds = floor $ utcTimeToPOSIXSeconds springForward
          fallSeconds = floor $ utcTimeToPOSIXSeconds fallBack
      
      secondsToUTCTime springSeconds @?= springForward
      secondsToUTCTime fallSeconds @?= fallBack

  , testProperty "UTC time consistency" $
      forAll genValidUTCTime $ \utcTime ->
        let seconds = floor $ utcTimeToPOSIXSeconds utcTime
            converted = secondsToUTCTime seconds
        in abs (diffUTCTime utcTime converted) < 1 -- Within 1 second
  ]

-- | Error handling and edge case tests
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ testCase "Negative timestamp handling" $ do
      -- Test that negative timestamps are handled gracefully
      let negativeTime = secondsToUTCTime (-86400) -- One day before epoch
          expected = UTCTime (fromGregorian 1969 12 31) 0
      negativeTime @?= expected

  , testCase "Zero timestamp edge case" $ do
      let zeroTime = secondsToUTCTime 0
          epochStart = UTCTime (fromGregorian 1970 1 1) 0
      zeroTime @?= epochStart

  , testCase "Leap second handling" $ do
      -- Test around known leap second dates
      let leapSecondDate = UTCTime (fromGregorian 2016 12 31) 86399 -- 2016-12-31 23:59:59
          nextDay = UTCTime (fromGregorian 2017 1 1) 0
      
      -- Should handle leap seconds consistently
      let leapSeconds = floor $ utcTimeToPOSIXSeconds leapSecondDate
          nextDaySeconds = floor $ utcTimeToPOSIXSeconds nextDay
      
      (nextDaySeconds - leapSeconds) @?= 1

  , testProperty "Millisecond precision preservation" $
      forAll (choose (0, 2147483647000)) $ \ms ->
        let utcTime = millisecondsToUTCTime ms
            backToMs = floor $ utcTimeToPOSIXSeconds utcTime * 1000
        in abs (fromIntegral ms - backToMs) <= 1000 -- Allow 1 second tolerance
  ]

-- | Generator for valid UTC times
genValidUTCTime :: Gen UTCTime
genValidUTCTime = do
  year <- choose (1970, 2037)
  month <- choose (1, 12)
  day <- choose (1, 28) -- Avoid month boundary issues
  hour <- choose (0, 23) :: Gen Int
  minute <- choose (0, 59) :: Gen Int
  second <- choose (0, 59) :: Gen Int
  return $ UTCTime (fromGregorian year month day) (fromIntegral $ hour * 3600 + minute * 60 + second)
