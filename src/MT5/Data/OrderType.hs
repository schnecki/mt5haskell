module MT5.Data.OrderType
    ( OrderType (..)
    , orderTypeToInt
    , intToOrderType
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

-- | Convert OrderType to MT5 integer value
--
-- MT5 uses sequential values (matches Haskell Enum):
-- - ORDER_TYPE_BUY             = 0
-- - ORDER_TYPE_SELL            = 1
-- - ORDER_TYPE_BUY_LIMIT       = 2
-- - ORDER_TYPE_SELL_LIMIT      = 3
-- - ORDER_TYPE_BUY_STOP        = 4
-- - ORDER_TYPE_SELL_STOP       = 5
-- - ORDER_TYPE_BUY_STOP_LIMIT  = 6
-- - ORDER_TYPE_SELL_STOP_LIMIT = 7
-- - ORDER_TYPE_CLOSE_BY        = 8
--
-- While this matches Enum deriving, we provide explicit conversion
-- for consistency and documentation.
orderTypeToInt :: OrderType -> Int
orderTypeToInt ORDER_TYPE_BUY             = 0
orderTypeToInt ORDER_TYPE_SELL            = 1
orderTypeToInt ORDER_TYPE_BUY_LIMIT       = 2
orderTypeToInt ORDER_TYPE_SELL_LIMIT      = 3
orderTypeToInt ORDER_TYPE_BUY_STOP        = 4
orderTypeToInt ORDER_TYPE_SELL_STOP       = 5
orderTypeToInt ORDER_TYPE_BUY_STOP_LIMIT  = 6
orderTypeToInt ORDER_TYPE_SELL_STOP_LIMIT = 7
orderTypeToInt ORDER_TYPE_CLOSE_BY        = 8

-- | Convert MT5 integer value to OrderType
intToOrderType :: Int -> OrderType
intToOrderType 0 = ORDER_TYPE_BUY
intToOrderType 1 = ORDER_TYPE_SELL
intToOrderType 2 = ORDER_TYPE_BUY_LIMIT
intToOrderType 3 = ORDER_TYPE_SELL_LIMIT
intToOrderType 4 = ORDER_TYPE_BUY_STOP
intToOrderType 5 = ORDER_TYPE_SELL_STOP
intToOrderType 6 = ORDER_TYPE_BUY_STOP_LIMIT
intToOrderType 7 = ORDER_TYPE_SELL_STOP_LIMIT
intToOrderType 8 = ORDER_TYPE_CLOSE_BY
intToOrderType _ = ORDER_TYPE_BUY  -- Default to BUY for unknown values
