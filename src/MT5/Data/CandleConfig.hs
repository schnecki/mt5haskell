{-# LANGUAGE OverloadedStrings #-}

-- | MT5-specific candle configuration types
--
-- Provides internal configuration types for MT5 candlestick queries,
-- following the principle of separation of concerns by keeping MT5-specific
-- logic isolated from external library types.
module MT5.Data.CandleConfig
    ( MT5CandleConfig(..)
    , CandleQueryType(..)
    -- , fromCandleConfig          -- COMMENTED OUT - external library function
    , validateMT5CandleConfig
    ) where

import           Data.Time
import           MT5.Data.Granularity

-- | MT5-specific candle configuration (internal use only)
--
-- Represents the three different query patterns supported by MT5:
-- range queries, count-from-date queries, and recent data queries.
-- Uses strict fields for performance and explicit query type for clarity.
data MT5CandleConfig = MT5CandleConfig
    { mt5Granularity :: !MT5Granularity  -- ^ Timeframe for candles
    , mt5QueryType   :: !CandleQueryType -- ^ Query pattern to use
    } deriving (Show, Eq)

-- | Different query types based on available parameters
--
-- Maps to specific MT5 Python API functions:
-- - RangeQuery -> copy_rates_range
-- - CountFromQuery -> copy_rates_from  
-- - RecentCountQuery -> copy_rates_from_pos
--
-- Following algebraic data types principle to make query patterns explicit.
data CandleQueryType = 
    RangeQuery !UTCTime !UTCTime              -- ^ from + to (copy_rates_range)
  | CountFromQuery !UTCTime !Int              -- ^ from + count (copy_rates_from)
  | RecentCountQuery !Int                     -- ^ count only (copy_rates_from_pos)
  deriving (Show, Eq)

{- COMMENTED OUT - To be implemented in other library that knows about CandleConfig
-- | Convert external CandleConfig to MT5CandleConfig, validating supported options
--
-- This bridge function would:
-- 1. Validate that only supported MT5 features are requested
-- 2. Convert external granularity types to MT5Granularity
-- 3. Map external query parameters to MT5CandleQueryType
-- 4. Return informative errors for unsupported features
--
-- Following the principle of explicit error handling using Either types.
fromCandleConfig :: CandleConfig -> Either String MT5CandleConfig
fromCandleConfig cfg = do
  -- Validation and conversion logic would go here
  undefined
-}

-- | Validate MT5-specific constraints
--
-- Ensures that MT5 API limits are respected, following the principle
-- of making illegal states unrepresentable at the type level where possible,
-- and using explicit validation where type-level constraints are insufficient.
--
-- ==== __Examples__
--
-- >>> validateMT5CandleConfig (MT5CandleConfig M5 (RecentCountQuery 100))
-- Right (MT5CandleConfig M5 (RecentCountQuery 100))
--
-- >>> validateMT5CandleConfig (MT5CandleConfig M5 (RecentCountQuery 10000))
-- Left "MT5 maximum count is 5000 candles per request"
validateMT5CandleConfig :: MT5CandleConfig -> Either String MT5CandleConfig
validateMT5CandleConfig cfg = case mt5QueryType cfg of
  CountFromQuery _ c | c > 5000 -> Left "MT5 maximum count is 5000 candles per request"
  CountFromQuery _ c | c <= 0   -> Left "MT5 count must be positive"
  RecentCountQuery c | c > 5000 -> Left "MT5 maximum count is 5000 candles per request"  
  RecentCountQuery c | c <= 0   -> Left "MT5 count must be positive"
  RangeQuery from to | to <= from -> Left "MT5 'to' time must be after 'from' time"
  _ -> Right cfg
