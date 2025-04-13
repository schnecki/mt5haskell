module MT5.Data.TradeRequestAction
    ( TradeRequestAction (..)

    ) where


data TradeRequestAction
  = TRADE_ACTION_DEAL     -- ^ Place an order for an instant deal with the specified parameters (set a market order)
  | TRADE_ACTION_PENDING  -- ^ Place an order for performing a deal at specified conditions (pending order)
  | TRADE_ACTION_SLTP     -- ^ Change open position Stop Loss and Take Profit
  | TRADE_ACTION_MODIFY   -- ^ Change parameters of the previously placed trading order
  | TRADE_ACTION_REMOVE   -- ^ Remove previously placed pending order
  | TRADE_ACTION_CLOSE_BY -- ^ Close a position by an opposite one
  deriving (Show, Eq, Enum)
