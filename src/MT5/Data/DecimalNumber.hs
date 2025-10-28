{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Precision-aware decimal numbers for trading.
--
-- This module provides a type-safe way to handle decimal numbers with correct
-- precision for financial applications (prices, volumes, etc.).
--
-- The 'DecimalNumber' type tracks both the numeric value and its precision metadata,
-- ensuring correct rounding and formatting.
module MT5.Data.DecimalNumber
  ( -- * Types
    DecimalNumber(..)
  , Precision
  , DecimalNumberError(..)
    -- * Smart Constructors
  , mkDecimalNumber
  , mkDecimalNumberFromDouble
    -- * Conversions
  , decimalNumberToDouble
  , setDecimalNumberPrecision
    -- * Rounding
  , roundDecimalNumber
    -- * Helpers
  , calculatePrecisionFromStep
  , roundToStep
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text        (pack, unpack)
import           GHC.Generics
import           Text.Printf

-- | Number of decimal places for precision.
type Precision = Int

-- | Precision-aware decimal number for trading operations.
--
-- The 'DecimalNumber' type tracks both the numeric value and its intended precision.
-- This ensures correct rounding and formatting for different use cases:
--
-- * Forex prices: 5 decimal places (e.g., 1.08523)
-- * Gold prices: 2 decimal places (e.g., 1850.25)
-- * Volumes: 2-4 decimal places (e.g., 0.01 lots)
-- * Stocks: 0 decimal places (whole shares)
--
-- ==== __Examples__
--
-- >>> mkDecimalNumber (Just 5) 1.08523
-- Right (DecimalNumber {decimalNumberPrecision = Just 5, fromDecimalNumber = 1.08523})
--
-- >>> mkDecimalNumber Nothing 100.5
-- Right (DecimalNumber {decimalNumberPrecision = Nothing, fromDecimalNumber = 100.5})
data DecimalNumber = DecimalNumber
  { decimalNumberPrecision :: !(Maybe Precision)  -- ^ Decimal places (Nothing = default 4)
  , fromDecimalNumber      :: !Double             -- ^ Numeric value
  } deriving (Show, Eq, Ord, Generic, NFData)

-- | Errors that can occur when working with decimal numbers.
data DecimalNumberError
  = InvalidPrecision Int
  | NegativeValue Double
  | ZeroValue
  | ValueBelowMinimum Double Double  -- ^ value, minimum
  | ValueAboveMaximum Double Double  -- ^ value, maximum
  | InvalidStep Double Double  -- ^ value, step
  deriving (Show, Eq, Generic, NFData)

-- | Smart constructor for DecimalNumber with validation.
--
-- Validates:
--
-- * Precision must be non-negative if provided
--
-- ==== __Examples__
--
-- >>> mkDecimalNumber (Just 2) 0.01
-- Right (DecimalNumber {decimalNumberPrecision = Just 2, fromDecimalNumber = 0.01})
--
-- >>> mkDecimalNumber (Just (-1)) 0.01
-- Left (InvalidPrecision (-1))
--
-- >>> mkDecimalNumber Nothing 100.5
-- Right (DecimalNumber {decimalNumberPrecision = Nothing, fromDecimalNumber = 100.5})
mkDecimalNumber :: Maybe Precision -> Double -> Either DecimalNumberError DecimalNumber
mkDecimalNumber prec val
  | Just p <- prec, p < 0 = Left (InvalidPrecision p)
  | otherwise  = Right (DecimalNumber prec val)

-- | Create DecimalNumber from Double without precision metadata.
--
-- Uses default precision (4 decimal places) for compatibility.
--
-- ==== __Examples__
--
-- >>> mkDecimalNumberFromDouble 0.01
-- Right (DecimalNumber {decimalNumberPrecision = Nothing, fromDecimalNumber = 0.01})
--
-- >>> mkDecimalNumberFromDouble (-1.5)
-- Right (DecimalNumber {decimalNumberPrecision = Nothing, fromDecimalNumber = -1.5})
mkDecimalNumberFromDouble :: Double -> Either DecimalNumberError DecimalNumber
mkDecimalNumberFromDouble = mkDecimalNumber Nothing

-- | Extract Double value from DecimalNumber (for backward compatibility).
--
-- ==== __Examples__
--
-- >>> decimalNumberToDouble (DecimalNumber (Just 2) 0.01)
-- 0.01
decimalNumberToDouble :: DecimalNumber -> Double
decimalNumberToDouble = fromDecimalNumber

-- | Set precision for a DecimalNumber, adjusting the value if necessary.
--
-- Rounds the value to the specified number of decimal places.
--
-- ==== __Examples__
--
-- >>> let num = DecimalNumber Nothing 1.08523
-- >>> setDecimalNumberPrecision 5 num
-- DecimalNumber {decimalNumberPrecision = Just 5, fromDecimalNumber = 1.08523}
--
-- >>> setDecimalNumberPrecision 2 num
-- DecimalNumber {decimalNumberPrecision = Just 2, fromDecimalNumber = 1.09}
setDecimalNumberPrecision :: Precision -> DecimalNumber -> DecimalNumber
setDecimalNumberPrecision prec num@(DecimalNumber _ val) =
  let factor = 10 ^ prec
      rounded = fromIntegral (round (val * factor) :: Integer) / factor
  in num { decimalNumberPrecision = Just prec, fromDecimalNumber = rounded }

-- | Round decimal number to a specific step.
--
-- Used to ensure value is an exact multiple of the step.
--
-- ==== __Examples__
--
-- >>> roundDecimalNumber 0.00001 (DecimalNumber Nothing 1.085234)
-- DecimalNumber {decimalNumberPrecision = Just 5, fromDecimalNumber = 1.08523}
--
-- >>> roundDecimalNumber 0.01 (DecimalNumber Nothing 100.456)
-- DecimalNumber {decimalNumberPrecision = Just 2, fromDecimalNumber = 100.46}
roundDecimalNumber :: Double -> DecimalNumber -> DecimalNumber
roundDecimalNumber step num@(DecimalNumber _ val)
  | step <= 0  = num  -- Invalid step, return unchanged
  | otherwise  =
      let steps = round (val / step) :: Integer
          rounded = fromIntegral steps * step
          -- Calculate precision from step
          prec = calculatePrecision step
      in num { decimalNumberPrecision = Just prec, fromDecimalNumber = rounded }
  where
    -- Calculate required decimal places from step size
    calculatePrecision s
      | s >= 1    = 0  -- Whole numbers
      | otherwise =
          let str = printf "%.10f" s :: String
              afterDecimal = dropWhile (/= '.') str
              digits = takeWhile (`elem` ("0123456789" :: String)) (drop 1 afterDecimal)
              -- Find first non-zero digit after decimal point
              leadingZeros = length $ takeWhile (== '0') digits
          in leadingZeros + 1

-- | JSON serialization.
--
-- Serializes to string with precision formatting:
--
-- * With precision: formatted to exact decimal places
-- * Without precision: default 4 decimal places
instance ToJSON DecimalNumber where
  toJSON (DecimalNumber (Just prec) val) =
    String $ pack (printf ("%." ++ show prec ++ "f") val :: String)
  toJSON (DecimalNumber Nothing val) =
    String $ pack (printf "%.4f" val :: String)

-- | JSON deserialization.
--
-- Accepts both string and number formats.
instance FromJSON DecimalNumber where
  parseJSON (String s) =
    case reads (unpack s) of
      [(val, "")] -> case mkDecimalNumberFromDouble val of
        Right num -> return num
        Left err  -> fail $ "Invalid decimal number: " ++ show err
      _           -> fail $ "Cannot parse decimal number from: " ++ unpack s
  parseJSON (Number n) =
    case mkDecimalNumberFromDouble (realToFrac n) of
      Right num -> return num
      Left err  -> fail $ "Invalid decimal number: " ++ show err
  parseJSON v = fail $ "Cannot parse decimal number from non-string/number: " ++ show v

-- | Numeric instances for convenience.
instance Num DecimalNumber where
  (DecimalNumber p1 v1) + (DecimalNumber p2 v2) =
    let prec = case (p1, p2) of
                 (Just p, _) -> Just p
                 (_, Just p) -> Just p
                 _           -> Nothing
        result = v1 + v2
    in DecimalNumber prec result

  (DecimalNumber p1 v1) * (DecimalNumber p2 v2) =
    let prec = case (p1, p2) of
                 (Just p, _) -> Just p
                 (_, Just p) -> Just p
                 _           -> Nothing
        result = v1 * v2
    in DecimalNumber prec result

  abs (DecimalNumber p v) = DecimalNumber p (abs v)
  signum (DecimalNumber p v) = DecimalNumber p (signum v)
  fromInteger i = DecimalNumber Nothing (fromInteger i)
  negate (DecimalNumber p v) = DecimalNumber p (negate v)

instance Fractional DecimalNumber where
  (DecimalNumber p1 v1) / (DecimalNumber p2 v2) =
    let prec = case (p1, p2) of
                 (Just p, _) -> Just p
                 (_, Just p) -> Just p
                 _           -> Nothing
        result = v1 / v2
    in DecimalNumber prec result

  fromRational r = DecimalNumber Nothing (fromRational r)

-- | Helper: Calculate precision from a step size.
--
-- Determines the number of decimal places needed to represent the step.
--
-- ==== __Examples__
--
-- >>> calculatePrecisionFromStep 0.01
-- 2
--
-- >>> calculatePrecisionFromStep 0.001
-- 3
--
-- >>> calculatePrecisionFromStep 1.0
-- 0
calculatePrecisionFromStep :: Double -> Precision
calculatePrecisionFromStep step
  | step >= 1.0 = 0
  | otherwise = length (takeWhile (/= '1') (drop 2 (show step)))
  where
    -- Alternative implementation using logarithms:
    -- ceiling (abs (logBase 10 step))

-- | Helper: Round a value to a specific step using banker's rounding.
--
-- Banker's rounding (round half to even) ensures consistent behavior:
--
-- * 1.25 → 1.2 (round down to even)
-- * 1.35 → 1.4 (round up to even)
--
-- ==== __Examples__
--
-- >>> roundToStep 0.01 1.556
-- 1.56
--
-- >>> roundToStep 0.1 1.25
-- 1.2
--
-- >>> roundToStep 1.0 1.5
-- 2.0
roundToStep :: Double -> Double -> Double
roundToStep step val =
  let rounded = fromIntegral (round (val / step) :: Integer) * step
      -- Handle floating point precision issues
      prec = calculatePrecisionFromStep step
      factor = 10.0 ^ prec
  in fromIntegral (round (rounded * factor) :: Integer) / factor
