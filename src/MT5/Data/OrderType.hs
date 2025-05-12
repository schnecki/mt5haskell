module MT5.Data.OrderType
    ( OrderType (..)
    ) where


data OrderType
  = ORDER_TYPE_BUY             -- ^ Market buy order
  | ORDER_TYPE_SELL            -- ^ Market sell order
  | ORDER_TYPE_BUY_LIMIT       -- ^ Buy Limit pending order
  | ORDER_TYPE_SELL_LIMIT      -- ^ Sell Limit pending order
  | ORDER_TYPE_BUY_STOP        -- ^ Buy Stop pending order
  | ORDER_TYPE_SELL_STOP       -- ^ Sell Stop pending order
  | ORDER_TYPE_BUY_STOP_LIMIT  -- ^ Upon reaching the order price, Buy Limit pending order is placed at StopLimit price
  | ORDER_TYPE_SELL_STOP_LIMIT -- ^ Upon reaching the order price, Sell Limit pending order is placed at StopLimit price
  | ORDER_TYPE_CLOSE_BY        -- ^ Order for closing a position by an opposite one
  deriving (Show, Eq, Enum)

