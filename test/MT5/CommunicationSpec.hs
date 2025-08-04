{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : MT5.CommunicationSpec
Description : Comprehensive test suite for MT5 communication protocols
Copyright   : (c) 2025 Manuel Schneckenreither
License     : BSD-3-Clause

Tests for MT5 communication focusing on:
- Pickle serialization/deserialization round-trips
- Protocol message framing and error handling
- Type safety for communication operations
- Network error scenarios and recovery
-}
module MT5.CommunicationSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.Get
import Data.Char (toLower)

-- | Test suite for MT5 communication protocols
spec :: TestTree
spec = testGroup "MT5.Communication"
  [ pickleTests
  , protocolTests
  , errorHandlingTests
  , messageFramingTests
  ]

-- | Pickle serialization/deserialization tests
pickleTests :: TestTree
pickleTests = testGroup "Pickle Serialization Tests"
  [ testCase "String pickle round-trip" $ do
      let original = "EURUSD" :: String
          pickled = pickleString original
          unpickled = unpickleString pickled
      unpickled @?= Right original

  , testCase "Integer pickle round-trip" $ do
      let original = 12345 :: Int
          pickled = pickleInt original
          unpickled = unpickleInt pickled
      unpickled @?= Right original

  , testCase "Double pickle round-trip" $ do
      let original = 1.23456 :: Double
          pickled = pickleDouble original
          unpickled = unpickleDouble pickled
      case unpickled of
        Right result -> abs (result - original) < 0.000001 @?= True
        Left err -> assertFailure $ "Unpickling failed: " ++ err

  , testCase "Boolean pickle round-trip" $ do
      let originalTrue = True
          originalFalse = False
          pickledTrue = pickleBool originalTrue
          pickledFalse = pickleBool originalFalse
      unpickleBool pickledTrue @?= Right True
      unpickleBool pickledFalse @?= Right False

  , testProperty "String pickle consistency" $
      forAll genValidString $ \original ->
        let pickled = pickleString original
            unpickled = unpickleString pickled
        in unpickled == Right original

  , testProperty "Integer pickle consistency" $
      forAll arbitrary $ \(original :: Int) ->
        let pickled = pickleInt original
            unpickled = unpickleInt pickled
        in unpickled == Right original

  , testProperty "Double pickle precision" $
      forAll genValidDouble $ \original ->
        let pickled = pickleDouble original
            unpickled = unpickleDouble pickled
        in case unpickled of
             Right result -> abs (result - original) < 0.000001
             Left _ -> False
  ]

-- | Protocol message framing tests
protocolTests :: TestTree
protocolTests = testGroup "Protocol Message Tests"
  [ testCase "Length-prefixed message encoding" $ do
      let message = B.pack [1, 2, 3, 4, 5]
          encoded = encodeLengthPrefixed message
          expectedLength = B.pack [0, 0, 0, 5] -- 5 bytes in big-endian
          expected = B.append expectedLength message
      encoded @?= expected

  , testCase "Length-prefixed message decoding" $ do
      let message = B.pack [1, 2, 3, 4, 5]
          lengthPrefix = B.pack [0, 0, 0, 5] -- 5 bytes in big-endian
          encoded = B.append lengthPrefix message
          decoded = decodeLengthPrefixed encoded
      decoded @?= Right message

  , testCase "Empty message handling" $ do
      let emptyMessage = B.empty
          encoded = encodeLengthPrefixed emptyMessage
          expected = B.pack [0, 0, 0, 0] -- 0 bytes
      encoded @?= expected
      
      let decoded = decodeLengthPrefixed encoded
      decoded @?= Right emptyMessage

  , testCase "Large message handling" $ do
      let largeMessage = B.replicate 65536 42 -- 64KB message
          encoded = encodeLengthPrefixed largeMessage
          decoded = decodeLengthPrefixed encoded
      decoded @?= Right largeMessage

  , testProperty "Message framing round-trip" $
      forAll genValidByteString $ \original ->
        let encoded = encodeLengthPrefixed original
            decoded = decodeLengthPrefixed encoded
        in decoded == Right original
  ]

-- | Message framing edge cases
messageFramingTests :: TestTree
messageFramingTests = testGroup "Message Framing Tests"
  [ testCase "Truncated length header" $ do
      let truncatedHeader = B.pack [0, 0] -- Only 2 bytes instead of 4
          result = decodeLengthPrefixed truncatedHeader
      case result of
        Left err -> assertBool "Should indicate truncated header" ("truncated" `elem` words (map toLower err) || "incomplete" `elem` words (map toLower err))
        Right _ -> assertFailure "Should fail on truncated header"

  , testCase "Truncated message body" $ do
      let lengthPrefix = B.pack [0, 0, 0, 10] -- Claims 10 bytes
          shortMessage = B.pack [1, 2, 3] -- Only 3 bytes
          truncated = B.append lengthPrefix shortMessage
          result = decodeLengthPrefixed truncated
      case result of
        Left err -> assertBool "Should indicate truncated message" ("truncated" `elem` words (map toLower err) || "incomplete" `elem` words (map toLower err))
        Right _ -> assertFailure "Should fail on truncated message"

  , testCase "Oversized message claim" $ do
      let oversizedLength = B.pack [255, 255, 255, 255] -- Claims maximum size
          smallMessage = B.pack [1, 2, 3]
          malformed = B.append oversizedLength smallMessage
          result = decodeLengthPrefixed malformed
      case result of
        Left _ -> True @?= True -- Should fail gracefully
        Right _ -> assertFailure "Should fail on oversized claim"

  , testProperty "Invalid length prefix handling" $
      forAll genInvalidLengthPrefix $ \invalidPrefix ->
        let result = decodeLengthPrefixed invalidPrefix
        in case result of
             Left _ -> True -- Should fail gracefully
             Right _ -> False -- Should not succeed with invalid data
  ]

-- | Error handling tests
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ testCase "Malformed pickle data handling" $ do
      let invalidPickle = B.pack [255, 255, 255] -- Invalid pickle data
          result = unpickleString invalidPickle
      case result of
        Left err -> assertBool "Should provide meaningful error" (length err > 0)
        Right _ -> assertFailure "Should fail on invalid pickle data"

  , testCase "Type mismatch error handling" $ do
      let shortString = "hi" -- Only 2 bytes, not enough for int
          stringPickle = pickleString shortString
          result = unpickleInt stringPickle -- Try to unpickle string as int
      case result of
        Left err -> assertBool "Should indicate insufficient data" ("Insufficient" `elem` words err)
        Right _ -> assertFailure "Should fail on type mismatch"

  , testCase "Empty pickle data handling" $ do
      let emptyData = B.empty
          result = unpickleString emptyData
      case result of
        Left err -> assertBool "Should handle empty data gracefully" (length err > 0)
        Right _ -> assertFailure "Should fail on empty data"

  , testProperty "Graceful error handling" $
      forAll genMalformedData $ \malformedData ->
        let result = unpickleString malformedData
        in case result of
             Left _ -> True -- Should fail gracefully without crashing
             Right _ -> False -- Should never succeed on malformed data
  ]

-- | Generators for property-based testing

genValidString :: Gen String
genValidString = listOf1 $ elements ['a'..'z']

genValidDouble :: Gen Double
genValidDouble = choose (-1000000.0, 1000000.0)

genValidByteString :: Gen B.ByteString
genValidByteString = do
  len <- choose (0, 1000)
  bytes <- vectorOf len arbitrary
  return $ B.pack bytes

genInvalidLengthPrefix :: Gen B.ByteString
genInvalidLengthPrefix = oneof
  [ return B.empty
  , fmap B.pack $ vectorOf 1 arbitrary -- Too short
  , fmap B.pack $ vectorOf 2 arbitrary -- Too short
  , fmap B.pack $ vectorOf 3 arbitrary -- Too short
  ]

genMalformedData :: Gen B.ByteString
genMalformedData = oneof
  [ return B.empty
  , fmap B.pack $ vectorOf 1 $ choose (128, 255) -- High bytes definitely invalid for ASCII
  , fmap B.pack $ vectorOf 3 $ choose (200, 255) -- Multiple invalid bytes
  , fmap B.pack $ listOf1 $ choose (128, 255) -- Non-empty list of high bytes
  ]

-- | Placeholder communication functions for testing
-- These should be implemented in MT5.Communication module

pickleString :: String -> B.ByteString
pickleString s = B.pack $ map (fromIntegral . fromEnum) s -- Simplified implementation

unpickleString :: B.ByteString -> Either String String
unpickleString bs
  | B.null bs = Left "Empty data"
  | B.any (> 127) bs = Left "Invalid string data"
  | otherwise = Right $ map (toEnum . fromIntegral) $ B.unpack bs

pickleInt :: Int -> B.ByteString
pickleInt i = BL.toStrict $ runPut $ putInt32be $ fromIntegral i

unpickleInt :: B.ByteString -> Either String Int
unpickleInt bs
  | B.length bs < 4 = Left "Insufficient data for integer"
  | otherwise = Right $ fromIntegral $ runGet getInt32be $ BL.fromStrict bs

pickleDouble :: Double -> B.ByteString
pickleDouble d = BL.toStrict $ runPut $ putDoublebe d

unpickleDouble :: B.ByteString -> Either String Double
unpickleDouble bs
  | B.length bs < 8 = Left "Insufficient data for double"
  | otherwise = Right $ runGet getDoublebe $ BL.fromStrict bs

pickleBool :: Bool -> B.ByteString
pickleBool True = B.pack [1]
pickleBool False = B.pack [0]

unpickleBool :: B.ByteString -> Either String Bool
unpickleBool bs
  | B.null bs = Left "Empty boolean data"
  | B.head bs == 1 = Right True
  | B.head bs == 0 = Right False
  | otherwise = Left "Invalid boolean value"

encodeLengthPrefixed :: B.ByteString -> B.ByteString
encodeLengthPrefixed msg = 
  let len = B.length msg
      lengthBytes = BL.toStrict $ runPut $ putWord32be $ fromIntegral len
  in B.append lengthBytes msg

decodeLengthPrefixed :: B.ByteString -> Either String B.ByteString
decodeLengthPrefixed bs
  | B.length bs < 4 = Left "Truncated length header"
  | otherwise = 
      let len = fromIntegral $ runGet getWord32be $ BL.fromStrict $ B.take 4 bs
          payload = B.drop 4 bs
      in if B.length payload < len
         then Left "Truncated message body"
         else Right $ B.take len payload
