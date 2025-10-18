{-# LANGUAGE OverloadedStrings #-}

-- | File corruption handling tests for MT5 file communication.
module MT5.Integration.FileCorruptionSpec (fileCorruptionTests) where

import           Test.Hspec
import           Data.Aeson (eitherDecode, Value)
import qualified Data.ByteString.Lazy as BSL

fileCorruptionTests :: Spec
fileCorruptionTests = describe "File Corruption Handling Tests" $ do  
  describe "Partial Write Detection" $ do
    it "detects incomplete JSON (missing closing brace)" $ do
      let partialJSON = "{\"success\":true,\"data\":" :: BSL.ByteString
      let parseResult = eitherDecode partialJSON :: Either String Value
      case parseResult of
        Left _ -> return ()
        Right _ -> expectationFailure "Should fail on incomplete JSON"        
    it "detects truncated JSON (cut off mid-field)" $ do
      let truncatedJSON = "{\"success\":true,\"data\":{\"login\":12" :: BSL.ByteString
      let parseResult = eitherDecode truncatedJSON :: Either String Value
      case parseResult of
        Left _ -> return ()
        Right _ -> expectationFailure "Should fail on truncated JSON"      
  describe "Invalid JSON Recovery" $ do
    it "handles empty JSON object" $ do
      let emptyJSON = "{}" :: BSL.ByteString
      let parseResult = eitherDecode emptyJSON :: Either String Value
      case parseResult of
        Right _ -> return ()
        Left err -> expectationFailure $ "Should parse empty object: " ++ err      
  describe "Character Encoding Issues" $ do
    it "handles UTF-8 encoded symbols correctly" $ do
      let utf8JSON = "{\"success\":true,\"data\":{\"symbol\":\"EUR/USD\"}}" :: BSL.ByteString
      let parseResult = eitherDecode utf8JSON :: Either String Value
      case parseResult of
        Right _ -> return ()
        Left err -> expectationFailure $ "Should parse UTF-8 JSON: " ++ err
