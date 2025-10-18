{-# LANGUAGE OverloadedStrings #-}

-- | Error recovery and retry logic tests.
module MT5.Integration.ErrorRecoverySpec (errorRecoveryTests) where

import           Test.Hspec
import           MT5.Error
import           Control.Concurrent (threadDelay)
import qualified Data.Text as T

errorRecoveryTests :: Spec
errorRecoveryTests = describe "Error Recovery Tests" $ do  
  describe "Timeout Handling" $ do
    it "withTimeout enforces timeout limit" $ do
      result <- withTimeout 100 "test_op" $ threadDelay 200000
      -- Check that it's a timeout/unknown error (exact type varies by implementation)
      case result of
        Left (TimeoutError _ _) -> return ()
        Left (UnknownError msg) | "timeout" `T.isInfixOf` T.toLower msg -> return ()
        Left err -> expectationFailure $ "Expected timeout error, got: " ++ show err
        Right _ -> expectationFailure "Expected timeout error"     
    it "withTimeout succeeds when operation completes quickly" $ do
      result <- withTimeout 1000 "quick_op" $ return (42 :: Int)
      result `shouldBe` Right 42      
  describe "File Access Errors" $ do
    it "FileAccessError is retryable" $ do
      let err = FileAccessError "/tmp/missing.json"
      isRetryableError err `shouldBe` True      
  describe "Non-Retryable Errors" $ do
    it "ValidationError is not retryable" $ do
      let err = ValidationError "Invalid parameter"
      isRetryableError err `shouldBe` False      
    it "InsufficientFunds is not retryable" $ do
      let err = InsufficientFunds 1000.0 500.0
      isRetryableError err `shouldBe` False
