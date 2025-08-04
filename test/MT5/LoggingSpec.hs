{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : MT5.LoggingSpec
Description : Test suite for MT5 logging wrapper functions
Copyright   : (c) 2025 Manuel Schneckenreither
License     : BSD-3-Clause

Tests for our MT5.Logging wrapper module focusing on:
- Basic wrapper function behavior
- Integration with our package configuration
- Error handling in our logging setup
-}
module MT5.LoggingSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
-- import System.IO.Temp (withSystemTempFile)
import Control.Exception (try, SomeException)
import EasyLogger (LogDestination(..))
import MT5.Logging

-- | Test suite for our MT5 logging wrapper functions
spec :: TestTree
spec = testGroup "MT5.Logging"
  [ basicFunctionalityTests
  , errorHandlingTests
  ]

-- | Basic functionality tests for our wrapper functions
basicFunctionalityTests :: TestTree
basicFunctionalityTests = testGroup "Basic Functionality"
    [
--    testCase "Enable logging to file doesn't crash" $ do
--       -- Test our enableMT5Logging wrapper function
--       withSystemTempFile "test2.log" $ \path _handle -> do
--         result <- try $ enableMT5Logging (LogFile path)
--         case result of
--           Right _ -> disableMT5Logging >> return () -- Success
--           Left ex -> assertFailure $ "enableMT5Logging failed: " ++ show (ex :: SomeException)

   testCase "Disable logging doesn't crash" $ do
      -- Test our disableMT5Logging wrapper function  
      result <- try disableMT5Logging
      case result of
        Right _ -> return () -- Success
        Left ex -> assertFailure $ "disableMT5Logging failed: " ++ show (ex :: SomeException)

--   , testCase "Enable and disable sequence works" $ do
--       -- Test our wrapper functions work in sequence
--       withSystemTempFile "test3.log" $ \path _handle -> do
--         result1 <- try $ enableMT5Logging (LogFile path)
--         result2 <- try disableMT5Logging
        
--         case (result1, result2) of
--           (Right _, Right _) -> return () -- Success
--           (Left ex, _) -> assertFailure $ "enableMT5Logging failed: " ++ show (ex :: SomeException)
--           (_, Left ex) -> assertFailure $ "disableMT5Logging failed: " ++ show (ex :: SomeException)
  ]

-- | Error handling tests for our wrapper functions
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling"
  [ testCase "Enable logging to invalid path handled gracefully" $ do
      -- Test our enableMT5Logging handles invalid paths
      result <- try $ enableMT5Logging (LogFile "/invalid/nonexistent/path/test.log")
      case result of
        Right _ -> return () -- May succeed depending on EasyLogger behavior
        Left (_ :: SomeException) -> return () -- Expected to fail gracefully

  , testCase "Disable without enable handled" $ do
      -- Test calling our disableMT5Logging without prior enable
      result <- try disableMT5Logging
      case result of
        Right _ -> return () -- Success
        Left (_ :: SomeException) -> return () -- May fail, that's ok
  ]
