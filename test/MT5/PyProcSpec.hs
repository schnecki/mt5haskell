{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MT5.PyProcSpec
Description : Test suite for PyProc data type and management
Copyright   : (c) 2025 Manuel Schneckenreither
License     : BSD-3-Clause

Tests for PyProc data type focusing on construction, IORef management,
and uninitialised-access error handling.
-}
module MT5.PyProcSpec (spec) where

import           Data.IORef
import           System.IO        (Handle, IOMode (..), hClose, openFile)
import           Test.Tasty
import           Test.Tasty.HUnit

import           MT5.Communication.PyProc


-- | Test suite for PyProc data type and management
spec :: TestTree
spec = testGroup "MT5.PyProc"
  [ dataTypeTests
  , ioRefTests
  , errorHandlingTests
  ]

-- | Open two /dev/null handles to stand in for the socket handles in tests.
withDevNull :: ((Handle, Handle) -> IO a) -> IO a
withDevNull f = do
    r <- openFile "/dev/null" ReadMode
    w <- openFile "/dev/null" WriteMode
    f (r, w) <* (hClose r >> hClose w)

-- | Data type construction and access tests
dataTypeTests :: TestTree
dataTypeTests = testGroup "PyProc Data Type"
  [ testCase "PyProc construction stores handles" $ withDevNull $ \(r, w) -> do
      let pp = PyProc r w (return ())
      pyIn  pp @?= r
      pyOut pp @?= w

  , testCase "PyProc field access does not throw" $ withDevNull $ \(r, w) -> do
      let pp = PyProc r w (return ())
      pyIn  pp `seq` return ()
      pyOut pp `seq` return ()
  ]

-- | IORef management tests
ioRefTests :: TestTree
ioRefTests = testGroup "IORef Management"
  [ testCase "pyProc IORef can be written and read" $ withDevNull $ \(r, w) -> do
      let pp = PyProc r w (return ())
      saved <- readIORef pyProc
      writeIORef pyProc (Just pp)
      result <- readIORef pyProc
      writeIORef pyProc saved
      case result of
        Nothing  -> assertFailure "pyProc should contain a value after writing"
        Just pp' -> do
          pyIn  pp' @?= r
          pyOut pp' @?= w

  , testCase "pyProc IORef can be updated" $ do
      r1 <- openFile "/dev/null" ReadMode
      w1 <- openFile "/dev/null" WriteMode
      r2 <- openFile "/dev/null" ReadMode
      w2 <- openFile "/dev/null" WriteMode
      saved <- readIORef pyProc
      writeIORef pyProc (Just (PyProc r1 w1 (return ())))
      writeIORef pyProc (Just (PyProc r2 w2 (return ())))
      result <- readIORef pyProc
      writeIORef pyProc saved
      hClose r1 >> hClose w1 >> hClose r2 >> hClose w2
      case result of
        Nothing  -> assertFailure "pyProc should not be Nothing"
        Just pp' -> pyIn pp' @?= r2
  ]

-- | Error handling tests
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling"
  [ testCase "Reading uninitialised pyProc returns Nothing" $ do
      saved <- readIORef pyProc
      writeIORef pyProc Nothing
      result <- readIORef pyProc
      writeIORef pyProc saved
      case result of
        Nothing -> return ()
        Just _  -> assertFailure "pyProc should be Nothing when uninitialised"
  ]
