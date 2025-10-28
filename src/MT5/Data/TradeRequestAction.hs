module MT5.Data.TradeRequestAction
    ( TradeRequestAction (..)
    , tradeRequestActionToInt
    , intToTradeRequestAction
    ) where


data TradeRequestAction
  = TRADE_ACTION_DEAL     -- ^ Place an order for an instant deal with the specified parameters (set a market order)
  | TRADE_ACTION_PENDING  -- ^ Place an order for performing a deal at specified conditions (pending order)
  | TRADE_ACTION_SLTP     -- ^ Change open position Stop Loss and Take Profit
  | TRADE_ACTION_MODIFY   -- ^ Change parameters of the previously placed trading order
  | TRADE_ACTION_REMOVE   -- ^ Remove previously placed pending order
  | TRADE_ACTION_CLOSE_BY -- ^ Close a position by an opposite one
  deriving (Show, Eq, Enum)

-- | Convert TradeRequestAction to MT5 integer value
--
-- MT5 ACTUAL values (verified via PrintEnumValues.mq5 script):
-- - TRADE_ACTION_DEAL     = 1  (instant execution)
-- - TRADE_ACTION_PENDING  = 5  (pending order)
-- - TRADE_ACTION_SLTP     = 6  (modify SL/TP)
-- - TRADE_ACTION_MODIFY   = 7  (modify pending order)
-- - TRADE_ACTION_REMOVE   = 8  (remove pending order)
-- - TRADE_ACTION_CLOSE_BY = 10 (close by opposite position)
--
-- These are NON-SEQUENTIAL and do NOT match Haskell's Enum deriving!
tradeRequestActionToInt :: TradeRequestAction -> Int
tradeRequestActionToInt TRADE_ACTION_DEAL     = 1   -- MT5 verified
tradeRequestActionToInt TRADE_ACTION_PENDING  = 5   -- MT5 verified
tradeRequestActionToInt TRADE_ACTION_SLTP     = 6   -- MT5 verified (was 2 - WRONG!)
tradeRequestActionToInt TRADE_ACTION_MODIFY   = 7   -- MT5 verified (was 3 - WRONG!)
tradeRequestActionToInt TRADE_ACTION_REMOVE   = 8   -- MT5 verified (was 4 - WRONG!)
tradeRequestActionToInt TRADE_ACTION_CLOSE_BY = 10  -- MT5 verified

-- | Convert MT5 integer value to TradeRequestAction
intToTradeRequestAction :: Int -> TradeRequestAction
intToTradeRequestAction 1  = TRADE_ACTION_DEAL
intToTradeRequestAction 5  = TRADE_ACTION_PENDING
intToTradeRequestAction 6  = TRADE_ACTION_SLTP     -- Fixed from 2
intToTradeRequestAction 7  = TRADE_ACTION_MODIFY   -- Fixed from 3
intToTradeRequestAction 8  = TRADE_ACTION_REMOVE   -- Fixed from 4
intToTradeRequestAction 10 = TRADE_ACTION_CLOSE_BY
intToTradeRequestAction _  = TRADE_ACTION_DEAL  -- Default to DEAL for unknown values
