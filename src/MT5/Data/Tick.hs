{-# LANGUAGE OverloadedStrings #-}

-- | MT5 tick data structures
module MT5.Data.Tick
    ( MT5Tick(..)
    , copyTicksFlagsAll
    , copyTicksFlagsInfo
    , copyTicksFlagsTrade
    ) where

import           Data.Time (UTCTime)

-- | A single market tick as returned by MT5's copy_ticks_from / copy_ticks_range.
--
-- MT5 ticks carry both bid and ask at millisecond resolution, making them the
-- authoritative source for spread computation when building bid/ask OHLC candles.
data MT5Tick = MT5Tick
    { mt5TickTimeMsc    :: !Integer  -- ^ Timestamp in milliseconds since Unix epoch
    , mt5TickBid        :: !Double   -- ^ Bid price
    , mt5TickAsk        :: !Double   -- ^ Ask price
    , mt5TickLast       :: !Double   -- ^ Last trade price
    , mt5TickVolume     :: !Int      -- ^ Tick volume
    , mt5TickFlags      :: !Int      -- ^ Tick type flags (see COPY_TICKS_* constants)
    , mt5TickVolumeReal :: !Double   -- ^ Real traded volume
    } deriving (Show, Eq)

-- | MT5 COPY_TICKS_ALL flag: retrieve all tick types.
copyTicksFlagsAll :: Int
copyTicksFlagsAll = 1

-- | MT5 COPY_TICKS_INFO flag: retrieve quote ticks (bid/ask) only.
copyTicksFlagsInfo :: Int
copyTicksFlagsInfo = 2

-- | MT5 COPY_TICKS_TRADE flag: retrieve trade ticks only.
copyTicksFlagsTrade :: Int
copyTicksFlagsTrade = 4
