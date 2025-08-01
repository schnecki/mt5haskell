{-# LANGUAGE OverloadedStrings #-}

-- | MT5 candlestick data structures
--
-- Provides MT5-specific data types for OHLC candlestick data,
-- following the established naming conventions and strict field evaluation
-- for optimal performance in trading applications.
module MT5.Data.Candle
    ( MT5Candle(..)
    , MT5CandleData(..)
    -- , fromMT5Candles            -- COMMENTED OUT - external library function
    ) where

import           Data.Time

-- | Individual MT5 candlestick data point
--
-- Represents a single OHLC candle with volume data as provided by
-- the MetaTrader5 Python API. Uses strict fields for performance
-- and precise types following the principle of making illegal states
-- unrepresentable.
--
-- The naming convention follows the established codebase pattern
-- with 'mt5' prefix for MT5-specific types.
--
-- ==== __Examples__
--
-- >>> let candle = MT5Candle utcTime 1.0850 1.0855 1.0848 1.0852 1500
-- >>> mt5CandleHigh candle - mt5CandleLow candle
-- 7.0e-4
data MT5Candle = MT5Candle
    { mt5CandleTime   :: !UTCTime  -- ^ Candle timestamp (in UTC)
    , mt5CandleOpen   :: !Double   -- ^ Opening price
    , mt5CandleHigh   :: !Double   -- ^ Highest price during period
    , mt5CandleLow    :: !Double   -- ^ Lowest price during period
    , mt5CandleClose  :: !Double   -- ^ Closing price
    , mt5CandleVolume :: !Int      -- ^ Tick volume (number of ticks)
    } deriving (Show, Eq)

-- | Response wrapper for MT5 candle data
--
-- Contains a collection of candles along with the symbol they represent.
-- Uses list instead of Vector for simplicity and compatibility with
-- existing codebase patterns.
--
-- Following the principle of explicit data flow - the symbol is
-- carried along with the data to maintain context.
data MT5CandleData = MT5CandleData
    { mt5Candles :: ![MT5Candle]   -- ^ List of candles (chronologically ordered)
    , mt5Symbol  :: !String        -- ^ Trading symbol (e.g., "EURUSD")
    } deriving (Show, Eq)

{- COMMENTED OUT - To be implemented in other library that knows about Candles ExchangeK
-- | Convert MT5Candle data to generic Candles format
--
-- This bridge function would handle conversion from MT5-specific types
-- to the external library's generic candle representation. The conversion
-- would need to handle:
-- 1. Time zone conversion if needed
-- 2. Price scaling or normalization
-- 3. Volume unit conversion
-- 4. Any additional metadata mapping
--
-- Following the principle of explicit error handling and referential transparency.
fromMT5Candles :: MT5CandleData -> Candles ExchangeK
fromMT5Candles mt5Data = undefined
-}
