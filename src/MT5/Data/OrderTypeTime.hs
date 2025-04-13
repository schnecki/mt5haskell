module MT5.Data.OrderTypeTime
  ( OrderTypeTime(..)
  ) where


data OrderTypeTime =
    ORDER_TIME_GTC           -- ^ The order stays in the queue until it is manually canceled
  | ORDER_TIME_DAY           -- ^ The order is active only during the current trading day
  | ORDER_TIME_SPECIFIED     -- ^ The order is active until the specified date
  | ORDER_TIME_SPECIFIED_DAY -- ^ The order is active until 23:59:59 of the specified day. If this time appears to be
                             -- out of a trading session, the expiration is processed at the nearest trading time.
  deriving (Show, Eq, Enum)
