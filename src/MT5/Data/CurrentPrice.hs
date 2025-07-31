{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Current price information for trading symbols
--
-- This module provides data types and utilities for handling real-time
-- price information from MT5, including bid/ask prices, spreads, and
-- tick flag information.
module MT5.Data.CurrentPrice 
  ( CurrentPrice(..)
  , calculateSpread
  , isValidPrice
  , hasTickFlag
  , tickFlagBid, tickFlagAsk, tickFlagLast
  , tickFlagVolume, tickFlagBuy, tickFlagSell
  ) where

import           Control.DeepSeq
import           Data.Bits       ((.&.))
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           GHC.Generics

-- | Current price information for a trading symbol
-- Based on the MT5 Tick structure fields
data CurrentPrice = CurrentPrice
  { cpSymbol     :: !Text      -- ^ Trading symbol (e.g., "EURUSD")
  , cpBid        :: !Double    -- ^ Current bid price (sell price)
  , cpAsk        :: !Double    -- ^ Current ask price (buy price)  
  , cpSpread     :: !Double    -- ^ Spread (ask - bid)
  , cpLast       :: !Double    -- ^ Last deal price (may be 0.0)
  , cpVolume     :: !Int       -- ^ Tick volume
  , cpTime       :: !UTCTime   -- ^ Last update time (UTC)
  , cpTimeMsc    :: !Integer   -- ^ Time in milliseconds since epoch
  , cpFlags      :: !Int       -- ^ Tick flags
  , cpVolumeReal :: !Double    -- ^ Real volume
  } deriving (Show, Eq, Generic, NFData)

-- | Calculate spread from bid/ask prices
calculateSpread :: Double -> Double -> Double
calculateSpread ask bid = ask - bid

-- | Check if price data is valid (non-zero bid/ask)
isValidPrice :: CurrentPrice -> Bool
isValidPrice cp = cpBid cp > 0 && cpAsk cp > 0

-- | Tick flag constants (matching MT5 library values)
tickFlagBid, tickFlagAsk, tickFlagLast, tickFlagVolume, tickFlagBuy, tickFlagSell :: Int
tickFlagBid    = 2   -- TICK_FLAG_BID: Bid price changed
tickFlagAsk    = 4   -- TICK_FLAG_ASK: Ask price changed
tickFlagLast   = 8   -- TICK_FLAG_LAST: Last price changed
tickFlagVolume = 16  -- TICK_FLAG_VOLUME: Volume changed
tickFlagBuy    = 32  -- TICK_FLAG_BUY: Last Buy price changed
tickFlagSell   = 64  -- TICK_FLAG_SELL: Last Sell price changed

-- | Check if specific tick flag is set
hasTickFlag :: CurrentPrice -> Int -> Bool
hasTickFlag cp flag = (cpFlags cp .&. flag) /= 0
