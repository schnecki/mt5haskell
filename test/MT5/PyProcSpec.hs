{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MT5.PyProcSpec
Description : Test suite for PyProc data type and management
Copyright   : (c) 2025 Manuel Schneckenreither
License     : BSD-3-Clause

Tests for our PyProc data type focusing on:
- Data type construction and field access
- IORef management behavior
- Error handling for uninitialized access
-}
module MT5.PyProcSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception (try, ErrorCall(..))
import Data.IORef
import System.IO    
import qualified Data.Text as T
import System.Process
import MT5.Communication.PyProc

-- | Test suite for PyProc data type and management
spec :: TestTree
spec = testGroup "MT5.PyProc"
  [ dataTypeTests
  , ioRefTests
  , errorHandlingTests
  ]

-- | Data type construction and access tests
dataTypeTests :: TestTree
dataTypeTests = testGroup "PyProc Data Type"
  [ testCase "PyProc construction with valid handles" $ do
      -- Create mock handles for testing
      (Just testIn, Just testOut, _, testHandle) <- createProcess $ (proc "echo" ["test"])
        { std_in = CreatePipe, std_out = CreatePipe }
      
      -- Test our PyProc constructor
      let pyProcess = PyProc testIn testOut testHandle "/fake/path"
      
      -- Test field accessors work (we can't test ProcessHandle equality)
      pyIn pyProcess @?= testIn
      pyOut pyProcess @?= testOut
      -- Note: ProcessHandle doesn't have Eq instance, so we validate it exists
      case pyProcHandle pyProcess of
        ph -> ph `seq` return () -- Just verify it's not bottom
      
      -- Cleanup
      terminateProcess testHandle
      hClose testIn
      hClose testOut

  , testCase "PyProc shows proper field access" $ do
      -- Create temporary process for testing
      (Just testIn, Just testOut, _, testHandle) <- createProcess $ (proc "echo" ["test"])
        { std_in = CreatePipe, std_out = CreatePipe }
      
      let pyProcess = PyProc testIn testOut testHandle "/fake/path"
      
      -- Test that we can access fields without error
      let inputHandle = pyIn pyProcess
      let outputHandle = pyOut pyProcess  
      let processHandle = pyProcHandle pyProcess
      
      -- These should not throw exceptions (ProcessHandle has no Eq instance)
      inputHandle @?= testIn
      outputHandle @?= testOut
      -- Just verify the process handle can be accessed
      case processHandle of
        ph -> ph `seq` return ()
      
      -- Cleanup
      terminateProcess testHandle
      hClose testIn
      hClose testOut
  ]

-- | IORef management tests
ioRefTests :: TestTree
ioRefTests = testGroup "IORef Management"
  [ testCase "pyProc IORef can be written and read" $ do
      -- Create test process
      (Just testIn, Just testOut, _, testHandle) <- createProcess $ (proc "echo" ["test"])
        { std_in = CreatePipe, std_out = CreatePipe }
      
      let testPyProc = PyProc testIn testOut testHandle "/fake/path"
      
      -- Test writing to our global IORef
      writeIORef pyProc testPyProc
      
      -- Test reading from our global IORef
      readPyProc <- readIORef pyProc
      
      -- Verify the data is the same (ProcessHandle has no Eq instance)
      pyIn readPyProc @?= testIn
      pyOut readPyProc @?= testOut
      -- Just verify the process handle can be accessed
      case pyProcHandle readPyProc of
        ph -> ph `seq` return ()
      
      -- Cleanup
      terminateProcess testHandle
      hClose testIn
      hClose testOut

  , testCase "pyProc IORef can be modified" $ do
      -- Create first test process
      (Just testIn1, Just testOut1, _, testHandle1) <- createProcess $ (proc "echo" ["test1"])
        { std_in = CreatePipe, std_out = CreatePipe }
      
      -- Create second test process  
      (Just testIn2, Just testOut2, _, testHandle2) <- createProcess $ (proc "echo" ["test2"])
        { std_in = CreatePipe, std_out = CreatePipe }
      
      let testPyProc1 = PyProc testIn1 testOut1 testHandle1 "/fake/path1"
      let testPyProc2 = PyProc testIn2 testOut2 testHandle2 "/fake/path2"
      
      -- Set first process
      writeIORef pyProc testPyProc1
      readPyProc1 <- readIORef pyProc
      pyIn readPyProc1 @?= testIn1
      
      -- Update to second process
      writeIORef pyProc testPyProc2
      readPyProc2 <- readIORef pyProc
      pyIn readPyProc2 @?= testIn2
      
      -- Cleanup
      terminateProcess testHandle1
      terminateProcess testHandle2
      hClose testIn1
      hClose testOut1
      hClose testIn2
      hClose testOut2
  ]

-- | Error handling tests
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling"
  [ testCase "Reading uninitialized pyProc throws error" $ do
      -- Reset the IORef to uninitialized state
      writeIORef pyProc (error "PyProc object was not set. You need to call startMT5!")
      
      -- Attempt to read should throw an error
      result <- try $ do
        pyprocess <- readIORef pyProc
        -- Force evaluation by accessing a field
        return $! pyIn pyprocess
      
      case result of
        Left (ErrorCall msg) -> 
          assertBool "Should mention PyProc not set" ("PyProc object was not set" `T.isInfixOf` T.pack msg)
        Right _ -> 
          assertFailure "Should have thrown an error for uninitialized PyProc"

  , testCase "Accessing fields of uninitialized PyProc fails appropriately" $ do
      -- Reset the IORef to uninitialized state
      writeIORef pyProc (error "PyProc object was not set. You need to call startMT5!")
      
      -- Test accessing pyIn field
      result <- try $ do
        pyprocess <- readIORef pyProc
        return $! pyIn pyprocess
      
      case result of
        Left (ErrorCall _) -> return () -- Expected
        Right _ -> assertFailure "Should fail when accessing uninitialized PyProc"
  ]
