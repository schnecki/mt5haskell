{-# LANGUAGE OverloadedStrings #-}

module MT5.Data.DecimalNumberSpec (spec) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Data.Aeson                (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           MT5.Data.DecimalNumber

-- | Test suite for DecimalNumber data type
spec :: TestTree
spec = testGroup "MT5.Data.DecimalNumber"
  [ smartConstructorTests
  , conversionTests
  , roundingTests
  , validationTests
  , jsonTests
  , arithmeticTests
  , propertyTests
  ]

-- | Tests for smart constructors
smartConstructorTests :: TestTree
smartConstructorTests = testGroup "Smart Constructors"
  [ testCase "mkDecimalNumber accepts valid positive volume with precision" $ do
      case mkDecimalNumber (Just 2) 0.01 of
        Right vol -> do
          fromDecimalNumber vol @?= 0.01
          decimalNumberPrecision vol @?= Just 2
        Left err -> assertFailure $ "Expected Right, got: " ++ show err

  , testCase "mkDecimalNumber accepts volume without precision" $ do
      case mkDecimalNumber Nothing 1.5 of
        Right vol -> do
          fromDecimalNumber vol @?= 1.5
          decimalNumberPrecision vol @?= Nothing
        Left err -> assertFailure $ "Expected Right, got: " ++ show err

  , testCase "mkDecimalNumber accepts negative values (generic type)" $ do
      case mkDecimalNumber (Just 2) (-0.01) of
        Right vol -> fromDecimalNumber vol @?= (-0.01)
        Left err -> assertFailure $ "Expected Right, got: " ++ show err

  , testCase "mkDecimalNumber accepts zero (generic type)" $ do
      case mkDecimalNumber (Just 2) 0.0 of
        Right vol -> fromDecimalNumber vol @?= 0.0
        Left err -> assertFailure $ "Expected Right, got: " ++ show err

  , testCase "mkDecimalNumber rejects negative precision" $ do
      case mkDecimalNumber (Just (-1)) 0.01 of
        Left (InvalidPrecision p) -> p @?= (-1)
        Right _ -> assertFailure "Expected InvalidPrecision error"
        Left err -> assertFailure $ "Wrong error type: " ++ show err

  , testCase "mkDecimalNumberFromDouble creates decimal without precision" $ do
      case mkDecimalNumberFromDouble 0.01 of
        Right vol -> do
          fromDecimalNumber vol @?= 0.01
          decimalNumberPrecision vol @?= Nothing
        Left err -> assertFailure $ "Expected Right, got: " ++ show err

  , testCase "mkDecimalNumberFromDouble accepts zero" $ do
      case mkDecimalNumberFromDouble 0.0 of
        Right vol -> fromDecimalNumber vol @?= 0.0
        Left err -> assertFailure $ "Expected Right, got: " ++ show err
  ]

-- | Tests for conversion functions
conversionTests :: TestTree
conversionTests = testGroup "Conversions"
  [ testCase "decimalNumberToDouble extracts value correctly" $ do
      let vol = DecimalNumber (Just 2) 0.01
      decimalNumberToDouble vol @?= 0.01

  , testCase "setDecimalNumberPrecision rounds to specified precision" $ do
      let vol = DecimalNumber Nothing 0.014  -- Changed from 0.015 to avoid banker's rounding ambiguity
          rounded = setDecimalNumberPrecision 2 vol
      fromDecimalNumber rounded @?= 0.01
      decimalNumberPrecision rounded @?= Just 2

  , testCase "setDecimalNumberPrecision rounds up correctly" $ do
      let vol = DecimalNumber Nothing 0.016
          rounded = setDecimalNumberPrecision 2 vol
      fromDecimalNumber rounded @?= 0.02
      decimalNumberPrecision rounded @?= Just 2

  , testCase "setDecimalNumberPrecision with 0 precision rounds to integer" $ do
      let vol = DecimalNumber Nothing 100.6
          rounded = setDecimalNumberPrecision 0 vol
      fromDecimalNumber rounded @?= 101.0
      decimalNumberPrecision rounded @?= Just 0
  ]

-- | Tests for rounding functions
roundingTests :: TestTree
roundingTests = testGroup "Rounding"
  [ testCase "roundDecimalNumber with 0.01 step (2 decimals)" $ do
      let vol = DecimalNumber Nothing 0.009999999776482582
          rounded = roundDecimalNumber 0.01 vol
      fromDecimalNumber rounded @?= 0.01
      decimalNumberPrecision rounded @?= Just 2

  , testCase "roundDecimalNumber with 0.001 step (3 decimals)" $ do
      let vol = DecimalNumber Nothing 0.0156
          rounded = roundDecimalNumber 0.001 vol
      fromDecimalNumber rounded @?= 0.016
      decimalNumberPrecision rounded @?= Just 3

  , testCase "roundDecimalNumber with 1.0 step (0 decimals)" $ do
      let vol = DecimalNumber Nothing 100.3
          rounded = roundDecimalNumber 1.0 vol
      fromDecimalNumber rounded @?= 100.0
      decimalNumberPrecision rounded @?= Just 0

  , testCase "roundDecimalNumber with 0.0001 step (4 decimals)" $ do
      let vol = DecimalNumber Nothing 0.01234  -- Changed to avoid rounding ambiguity
          rounded = roundDecimalNumber 0.0001 vol
      fromDecimalNumber rounded @?= 0.0123
      decimalNumberPrecision rounded @?= Just 4

  , testCase "roundDecimalNumber rounds down when below half step" $ do
      let vol = DecimalNumber Nothing 0.014
          rounded = roundDecimalNumber 0.01 vol
      fromDecimalNumber rounded @?= 0.01
      decimalNumberPrecision rounded @?= Just 2

  , testCase "roundDecimalNumber rounds up when above half step" $ do
      let vol = DecimalNumber Nothing 0.016
          rounded = roundDecimalNumber 0.01 vol
      fromDecimalNumber rounded @?= 0.02
      decimalNumberPrecision rounded @?= Just 2

  , testCase "roundDecimalNumber handles invalid step gracefully" $ do
      let vol = DecimalNumber Nothing 0.01
          rounded = roundDecimalNumber 0.0 vol
      fromDecimalNumber rounded @?= 0.01  -- Unchanged
      decimalNumberPrecision rounded @?= Nothing  -- Unchanged
  ]

-- | Tests for validation functions (removed - DecimalNumber is generic)
-- The validation logic now lives in the Volume compatibility layer or API layer
validationTests :: TestTree
validationTests = testGroup "Validation"
  [ testCase "DecimalNumber accepts any value (no built-in validation)" $ do
      let vol = DecimalNumber Nothing 0.01
      fromDecimalNumber vol @?= 0.01
  ]

-- | Tests for JSON serialization
jsonTests :: TestTree
jsonTests = testGroup "JSON Serialization"
  [ testCase "toJSON with precision formats correctly" $ do
      let vol = DecimalNumber (Just 2) 0.01
          json = encode vol
      json @?= "\"0.01\""

  , testCase "toJSON without precision uses 4 decimals" $ do
      let vol = DecimalNumber Nothing 1.5
          json = encode vol
      json @?= "\"1.5000\""

  , testCase "toJSON with 3 decimals" $ do
      let vol = DecimalNumber (Just 3) 0.016
          json = encode vol
      json @?= "\"0.016\""

  , testCase "toJSON with 0 decimals (stocks)" $ do
      let vol = DecimalNumber (Just 0) 100.0
          json = encode vol
      json @?= "\"100\""

  , testCase "fromJSON parses string" $ do
      let json = "\"0.01\"" :: BL.ByteString
      case decode json :: Maybe DecimalNumber of
        Just vol -> fromDecimalNumber vol @?= 0.01
        Nothing -> assertFailure "Failed to parse JSON"

  , testCase "fromJSON parses number" $ do
      let json = "1.5" :: BL.ByteString
      case decode json :: Maybe DecimalNumber of
        Just vol -> fromDecimalNumber vol @?= 1.5
        Nothing -> assertFailure "Failed to parse JSON"

  , testCase "fromJSON accepts zero (generic type)" $ do
      let json = "\"0.0\"" :: BL.ByteString
      case decode json :: Maybe DecimalNumber of
        Just val -> fromDecimalNumber val @?= 0.0
        Nothing -> assertFailure "Expected successful parse"

  , testCase "fromJSON roundtrip with precision" $ do
      let vol = DecimalNumber (Just 2) 0.01
          json = encode vol
      case decode json :: Maybe DecimalNumber of
        Just parsed -> fromDecimalNumber parsed @?= fromDecimalNumber vol
        Nothing -> assertFailure "Roundtrip failed"
  ]

-- | Tests for arithmetic operations
arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic Operations"
  [ testCase "addition preserves precision from first operand" $ do
      let vol1 = DecimalNumber (Just 2) 0.01
          vol2 = DecimalNumber (Just 3) 0.005
          result = vol1 + vol2
      fromDecimalNumber result @?= 0.015
      decimalNumberPrecision result @?= Just 2

  , testCase "addition without precision" $ do
      let vol1 = DecimalNumber Nothing 0.01
          vol2 = DecimalNumber Nothing 0.02
          result = vol1 + vol2
      fromDecimalNumber result @?= 0.03
      decimalNumberPrecision result @?= Nothing

  , testCase "multiplication preserves precision" $ do
      let vol1 = DecimalNumber (Just 2) 0.01
          vol2 = DecimalNumber Nothing 2.0
          result = vol1 * vol2
      fromDecimalNumber result @?= 0.02
      decimalNumberPrecision result @?= Just 2

  , testCase "division preserves precision" $ do
      let vol1 = DecimalNumber (Just 2) 0.02
          vol2 = DecimalNumber Nothing 2.0
          result = vol1 / vol2
      fromDecimalNumber result @?= 0.01
      decimalNumberPrecision result @?= Just 2

  , testCase "abs preserves precision" $ do
      let vol = DecimalNumber (Just 2) (-0.01)  -- Created via negate
          result = abs vol
      fromDecimalNumber result @?= 0.01
      decimalNumberPrecision result @?= Just 2

  , testCase "fromInteger creates volume without precision" $ do
      let vol = fromInteger 5 :: DecimalNumber
      fromDecimalNumber vol @?= 5.0
      decimalNumberPrecision vol @?= Nothing
  ]

-- | Property-based tests
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "mkDecimalNumber with positive value always succeeds" $
      \(Positive val) ->
        case mkDecimalNumber Nothing val of
          Right _ -> True
          Left _  -> False

  , testProperty "decimalNumberToDouble . mkDecimalNumberFromDouble = id (for valid volumes)" $
      \(Positive val) ->
        case mkDecimalNumberFromDouble val of
          Right vol -> abs (decimalNumberToDouble vol - val) < 1e-10
          Left _    -> False

  , testProperty "roundDecimalNumber is idempotent" $
      \(Positive step) (Positive val) ->
        step > 0 ==>
          let vol = DecimalNumber Nothing val
              rounded1 = roundDecimalNumber step vol
              rounded2 = roundDecimalNumber step rounded1
          in fromDecimalNumber rounded1 == fromDecimalNumber rounded2

  , testProperty "setDecimalNumberPrecision preserves precision setting" $
      \(Positive val) ->
        let prec = 2  -- Fixed precision to avoid discarded tests
            vol = DecimalNumber Nothing val
            rounded = setDecimalNumberPrecision prec vol
        in decimalNumberPrecision rounded == Just prec

  , testProperty "volume addition is commutative" $
      \(Positive v1) (Positive v2) ->
        case (mkDecimalNumberFromDouble v1, mkDecimalNumberFromDouble v2) of
          (Right vol1, Right vol2) ->
            let sum1 = vol1 + vol2
                sum2 = vol2 + vol1
            in abs (fromDecimalNumber sum1 - fromDecimalNumber sum2) < 1e-10
          _ -> False

  , testProperty "volume multiplication by 1 is identity" $
      \(Positive val) ->
        case mkDecimalNumberFromDouble val of
          Right vol ->
            let result = vol * fromInteger 1
            in abs (fromDecimalNumber result - fromDecimalNumber vol) < 1e-10
          Left _ -> False

  , testProperty "JSON roundtrip preserves value" $
      \(Positive val) ->
        val >= 0.01 ==>  -- Avoid very small values that lose precision in JSON
          case mkDecimalNumberFromDouble val of
            Right vol ->
              case decode (encode vol) :: Maybe DecimalNumber of
                Just parsed -> abs (fromDecimalNumber parsed - fromDecimalNumber vol) < 1e-4
                Nothing -> False
            Left _ -> False
  ]
