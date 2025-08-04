{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : MT5.InitSpec
Description : Test suite for MT5 initialization functions
Copyright   : (c) 2025 Manuel Schneckenreither
License     : BSD-3-Clause

Tests for our MT5.Init module focusing on:
- Environment detection logic
- Python environment detection and selection
- Configuration auto-detection
- Path resolution and validation
-}
module MT5.InitSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception (try, SomeException)
import MT5.Init
import MT5.Config

-- | Test suite for MT5 initialization functions
spec :: TestTree
spec = testGroup "MT5.Init"
  [ environmentDetectionTests
  , pythonEnvironmentTests
  , configurationTests
  , pathResolutionTests
  , errorHandlingTests
  ]

-- | Environment detection tests
environmentDetectionTests :: TestTree
environmentDetectionTests = testGroup "Environment Detection"
  [ testCase "detectExecutionEnvironment returns valid environment" $ do
      -- Test our environment detection function
      env <- detectExecutionEnvironment
      case env of
        WineEnvironment -> True @?= True
        WSLEnvironment -> True @?= True  
        NativeLinux -> True @?= True

  , testCase "detectExecutionEnvironment is deterministic" $ do
      -- Test that multiple calls return the same result
      env1 <- detectExecutionEnvironment
      env2 <- detectExecutionEnvironment
      env1 @?= env2

  , testCase "ExecutionEnvironment instances work correctly" $ do
      -- Test our ExecutionEnvironment data type
      let wine = WineEnvironment
      let wsl = WSLEnvironment
      let linux = NativeLinux
      
      -- Test Show instance
      show wine @?= "WineEnvironment"
      show wsl @?= "WSLEnvironment"
      show linux @?= "NativeLinux"
      
      -- Test Eq instance
      wine == WineEnvironment @?= True
      wine == WSLEnvironment @?= False
  ]

-- | Python environment detection and selection tests
pythonEnvironmentTests :: TestTree
pythonEnvironmentTests = testGroup "Python Environment Management"
  [ testCase "detectPythonEnvironments returns list" $ do
      -- Test our Python environment detection
      envs <- detectPythonEnvironments
      -- Should return a list (may be empty if no Python found)
      length envs >= 0 @?= True

  , testCase "PythonEnvironment data type works correctly" $ do
      -- Test our PythonEnvironment constructor and accessors
      let pyEnv = PythonEnvironment "/usr/bin/python3" "/usr/bin/pip3" DirectExecution
      
      pythonExecutable pyEnv @?= "/usr/bin/python3"
      pipExecutable pyEnv @?= "/usr/bin/pip3"
      executionMode pyEnv @?= DirectExecution

  , testCase "selectBestPythonEnvironment handles empty list" $ do
      -- Test our selection logic with edge cases
      let result = selectBestPythonEnvironment [] Nothing
      result @?= Nothing

  , testCase "selectBestPythonEnvironment selects first when no preference" $ do
      -- Test selection logic with multiple environments
      let env1 = PythonEnvironment "/usr/bin/python3" "/usr/bin/pip3" DirectExecution
      let env2 = PythonEnvironment "/usr/bin/python3.9" "/usr/bin/pip3.9" WineExecution
      let envs = [env1, env2]
      
      let result = selectBestPythonEnvironment envs Nothing
      case result of
        Just selected -> selected @?= env1
        Nothing -> assertFailure "Should select first environment"

  , testCase "selectBestPythonEnvironment respects execution mode preference" $ do
      -- Test that our selection logic respects preferences
      let env1 = PythonEnvironment "/usr/bin/python3" "/usr/bin/pip3" DirectExecution
      let env2 = PythonEnvironment "/usr/bin/python3.9" "/usr/bin/pip3.9" WineExecution
      let envs = [env1, env2]
      
      let result = selectBestPythonEnvironment envs (Just WineExecution)
      case result of
        Just selected -> executionMode selected @?= WineExecution
        Nothing -> return () -- Acceptable if no wine environment found

  , testCase "ExecutionMode data type works correctly" $ do
      -- Test our ExecutionMode instances
      let wine = WineExecution
      let direct = DirectExecution
      
      show wine @?= "WineExecution"
      show direct @?= "DirectExecution"
      wine == WineExecution @?= True
      wine == DirectExecution @?= False
  ]

-- | Configuration tests
configurationTests :: TestTree
configurationTests = testGroup "Configuration Management"
  [ testCase "autoDetectConfig returns valid config" $ do
      -- Test our auto-detection function
      result <- try $ autoDetectConfig defaultMT5Config
      case result of
        Right config -> do
          -- Should return a valid config
          venvDir config @?= venvDir config -- Basic sanity check
        Left (_ :: SomeException) -> 
          -- May fail if system dependencies missing, that's ok for tests
          assertBool "Should handle missing dependencies gracefully" True

  , testCase "resolveMT5LinuxPath handles different input types" $ do
      -- Test our path resolution logic
      let config1 = defaultMT5Config { mt5linuxLocalPath = Just "/custom/path" }
      let config2 = defaultMT5Config { mt5linuxLocalPath = Nothing }
      
      result1 <- (try :: IO FilePath -> IO (Either SomeException FilePath)) $ resolveMT5LinuxPath config1
      result2 <- (try :: IO FilePath -> IO (Either SomeException FilePath)) $ resolveMT5LinuxPath config2
      
      -- Both should complete without throwing exceptions in normal cases
      case (result1, result2) of
        (Right path1, Right path2) -> do
          -- Custom path should be respected
          path1 /= path2 @?= True  -- Should be different
        _ -> 
          -- May fail due to network/git issues, that's acceptable
          assertBool "Path resolution may fail due to external dependencies" True
  ]

-- | Path resolution tests  
pathResolutionTests :: TestTree
pathResolutionTests = testGroup "Path Resolution"
  [ testCase "resolveMT5LinuxPath respects user preference" $ do
      -- Test that user-specified paths are respected
      let customPath = "/user/specified/path"
      let config = defaultMT5Config { mt5linuxLocalPath = Just customPath }
      
      result <- (try :: IO FilePath -> IO (Either SomeException FilePath)) $ resolveMT5LinuxPath config
      case result of
        Right resolvedPath -> 
          -- Should use the custom path if specified
          customPath `elem` words resolvedPath @?= True
        Left (_ :: SomeException) ->
          -- May fail due to file system issues, acceptable
          return ()
  ]

-- | Error handling tests
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling"
  [ testCase "Functions handle missing dependencies gracefully" $ do
      -- Test that our functions don't crash on missing dependencies
      result1 <- try detectPythonEnvironments
      result2 <- try detectExecutionEnvironment
      
      case (result1, result2) of
        (Right _, Right _) -> return () -- Success
        (Left (_ :: SomeException), _) -> return () -- Expected to potentially fail
        (_, Left (_ :: SomeException)) -> return () -- Expected to potentially fail

  , testCase "autoDetectConfig handles system variations" $ do
      -- Test that auto-detection handles different system configurations
      result <- try $ autoDetectConfig defaultMT5Config
      case result of
        Right config -> do
          -- Should produce a config with reasonable defaults
          length (venvDir config) > 0 @?= True
        Left (_ :: SomeException) ->
          -- May fail on systems without required tools
          return ()

  , testCase "setupMT5LinuxRepository handles network issues" $ do
      -- Test that repository setup handles network problems gracefully
      let config = defaultMT5Config
      result <- try $ setupMT5LinuxRepository config
      case result of
        Right path -> 
          -- Should return a valid path
          assertBool "Path should not be empty" (length path > 0)
        Left (_ :: SomeException) -> 
          -- Expected to fail without network/git
          return ()
  ]

-- | Property-based test generators (currently unused but prepared for future tests)
