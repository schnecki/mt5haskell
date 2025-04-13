module MT5.Data.OrderTypeFilling
    ( OrderTypeFilling (..)
    ) where


data OrderTypeFilling
  = ORDER_FILLING_FOK    -- ^ This execution policy means that an order can be executed only in the specified volume. If
                         -- the necessary amount of a financial instrument is currently unavailable in the market, the
                         -- order will not be executed. The desired volume can be made up of several available offers.
  | ORDER_FILLING_IOC    -- ^ An agreement to execute a deal at the maximum volume available in the market within the
                         -- volume specified in the order. If the request cannot be filled completely, an order with the
                         -- available volume will be executed, and the remaining volume will be canceled.
  | ORDER_FILLING_RETURN -- ^ This policy is used only for market (ORDER_TYPE_BUY and ORDER_TYPE_SELL), limit and stop
                         -- limit orders (ORDER_TYPE_BUY_LIMIT, ORDER_TYPE_SELL_LIMIT, ORDER_TYPE_BUY_STOP_LIMIT and
                         -- ORDER_TYPE_SELL_STOP_LIMIT) and only for the symbols with Market or Exchange execution
                         -- modes. If filled partially, a market or limit order with the remaining volume is not
                         -- canceled, and is processed further. During activation of the ORDER_TYPE_BUY_STOP_LIMIT and
                         -- ORDER_TYPE_SELL_STOP_LIMIT orders, an appropriate limit order
                         -- ORDER_TYPE_BUY_LIMIT/ORDER_TYPE_SELL_LIMIT with the ORDER_FILLING_RETURN type is created.
  deriving (Show, Eq, Enum)
