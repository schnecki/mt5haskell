module MT5.Data.OrderTypeTime
  ( OrderTypeTime(..)
  , orderTypeTimeToInt
  , intToOrderTypeTime
  ) where


data OrderTypeTime =
    ORDER_TIME_GTC           -- ^ The order stays in the queue until it is manually canceled
  | ORDER_TIME_DAY           -- ^ The order is active only during the current trading day
  | ORDER_TIME_SPECIFIED     -- ^ The order is active until the specified date
  | ORDER_TIME_SPECIFIED_DAY -- ^ The order is active until 23:59:59 of the specified day. If this time appears to be
                             -- out of a trading session, the expiration is processed at the nearest trading time.
  deriving (Show, Eq, Enum)

-- | Convert OrderTypeTime to MT5 integer value
--
-- MT5 uses sequential values (matches Haskell Enum):
-- - ORDER_TIME_GTC           = 0
-- - ORDER_TIME_DAY           = 1
-- - ORDER_TIME_SPECIFIED     = 2
-- - ORDER_TIME_SPECIFIED_DAY = 3
--
-- While this matches Enum deriving, we provide explicit conversion
-- for consistency and documentation.
orderTypeTimeToInt :: OrderTypeTime -> Int
orderTypeTimeToInt ORDER_TIME_GTC           = 0
orderTypeTimeToInt ORDER_TIME_DAY           = 1
orderTypeTimeToInt ORDER_TIME_SPECIFIED     = 2
orderTypeTimeToInt ORDER_TIME_SPECIFIED_DAY = 3

-- | Convert MT5 integer value to OrderTypeTime
intToOrderTypeTime :: Int -> OrderTypeTime
intToOrderTypeTime 0 = ORDER_TIME_GTC
intToOrderTypeTime 1 = ORDER_TIME_DAY
intToOrderTypeTime 2 = ORDER_TIME_SPECIFIED
intToOrderTypeTime 3 = ORDER_TIME_SPECIFIED_DAY
intToOrderTypeTime _ = ORDER_TIME_GTC  -- Default to GTC for unknown values
