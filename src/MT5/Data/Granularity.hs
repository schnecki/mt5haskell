{-# LANGUAGE OverloadedStrings #-}

-- | MetaTrader5 timeframe definitions and conversion utilities
--
-- This module provides MT5-specific granularity types that map directly
-- to MetaTrader5 Python API timeframe constants, following the established
-- Haskell coding principles of precise type modeling and explicit data flow.
module MT5.Data.Granularity
    ( MT5Granularity(..)
    , toMT5TimeframeInt
    -- , fromCandlestickGranularity  -- COMMENTED OUT - external library function
    ) where

-- | MetaTrader5 supported timeframes
--
-- Maps directly to MT5 Python API timeframe constants for precise
-- type-driven development. Uses algebraic data types to make illegal
-- states unrepresentable - only valid MT5 timeframes can be expressed.
--
-- ==== __Examples__
--
-- >>> toMT5TimeframeInt M5
-- 5
--
-- >>> toMT5TimeframeInt H1
-- 16385
data MT5Granularity = 
    M1 | M2 | M3 | M4 | M5 | M6 | M10 | M12 | M15 | M20 | M30
  | H1 | H2 | H3 | H4 | H6 | H8 | H12 
  | D1 | W1 | MN1
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert MT5Granularity to MT5 timeframe integer constants
--
-- Maps Haskell types to the exact integer constants used by the
-- MetaTrader5 Python API. This ensures referential transparency
-- and eliminates magic numbers in the codebase.
--
-- Following the principle of explicit data flow - the mapping
-- between our types and MT5 constants is clearly visible.
toMT5TimeframeInt :: MT5Granularity -> Int
toMT5TimeframeInt granularity = case granularity of
  M1  -> 1      -- TIMEFRAME_M1
  M2  -> 2      -- TIMEFRAME_M2  
  M3  -> 3      -- TIMEFRAME_M3
  M4  -> 4      -- TIMEFRAME_M4
  M5  -> 5      -- TIMEFRAME_M5
  M6  -> 6      -- TIMEFRAME_M6
  M10 -> 10     -- TIMEFRAME_M10
  M12 -> 12     -- TIMEFRAME_M12
  M15 -> 15     -- TIMEFRAME_M15
  M20 -> 20     -- TIMEFRAME_M20
  M30 -> 30     -- TIMEFRAME_M30
  H1  -> 16385  -- TIMEFRAME_H1
  H2  -> 16386  -- TIMEFRAME_H2
  H3  -> 16387  -- TIMEFRAME_H3
  H4  -> 16388  -- TIMEFRAME_H4
  H6  -> 16390  -- TIMEFRAME_H6
  H8  -> 16392  -- TIMEFRAME_H8
  H12 -> 16396  -- TIMEFRAME_H12
  D1  -> 16408  -- TIMEFRAME_D1
  W1  -> 32769  -- TIMEFRAME_W1
  MN1 -> 49153  -- TIMEFRAME_MN1

{- COMMENTED OUT - To be implemented in other library that knows about CandlestickGranularity
-- | Convert from generic CandlestickGranularity to MT5Granularity
--
-- This bridge function would handle conversion from external library types
-- to MT5-specific types, but requires knowledge of the external type system.
fromCandlestickGranularity :: CandlestickGranularity -> Either String MT5Granularity
fromCandlestickGranularity = undefined
-}
