module MT5.Data.OrderSendResult
    ( OrderSendResult(..)
    , readOrderSendResult
    , tradeRetcodeNumber
    , explainTradeRetcodes
    , TradeRetcode
    ) where

import           MT5.Communication

data OrderSendResult = OrderSendResult
  { ordSendRetcode          :: TradeRetcode
  , ordSendDeal             :: Int
  , ordSendOrder            :: Int
  , ordSendVolume           :: Double
  , ordSendPrice            :: Double
  , ordSendBid              :: Double
  , ordSendAsk              :: Double
  , ordSendComment          :: String
  , ordSendRequest_id       :: Int
  , ordSendRetcode_external :: Int
  } deriving (Eq, Show)


readOrderSendResult :: IO OrderSendResult
readOrderSendResult =
  OrderSendResult
  <$> (toTradeRetcode . unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)


data TradeRetcode
  = TRADE_RETCODE_REQUOTE
  | TRADE_RETCODE_REJECT
  | TRADE_RETCODE_CANCEL
  | TRADE_RETCODE_PLACED
  | TRADE_RETCODE_DONE
  | TRADE_RETCODE_DONE_PARTIAL
  | TRADE_RETCODE_ERROR
  | TRADE_RETCODE_TIMEOUT
  | TRADE_RETCODE_INVALID
  | TRADE_RETCODE_INVALID_VOLUME
  | TRADE_RETCODE_INVALID_PRICE
  | TRADE_RETCODE_INVALID_STOPS
  | TRADE_RETCODE_TRADE_DISABLED
  | TRADE_RETCODE_MARKET_CLOSED
  | TRADE_RETCODE_NO_MONEY
  | TRADE_RETCODE_PRICE_CHANGED
  | TRADE_RETCODE_PRICE_OFF
  | TRADE_RETCODE_INVALID_EXPIRATION
  | TRADE_RETCODE_ORDER_CHANGED
  | TRADE_RETCODE_TOO_MANY_REQUESTS
  | TRADE_RETCODE_NO_CHANGES
  | TRADE_RETCODE_SERVER_DISABLES_AT
  | TRADE_RETCODE_CLIENT_DISABLES_AT
  | TRADE_RETCODE_LOCKED
  | TRADE_RETCODE_FROZEN
  | TRADE_RETCODE_INVALID_FILL
  | TRADE_RETCODE_CONNECTION
  | TRADE_RETCODE_ONLY_REAL
  | TRADE_RETCODE_LIMIT_ORDERS
  | TRADE_RETCODE_LIMIT_VOLUME
  | TRADE_RETCODE_INVALID_ORDER
  | TRADE_RETCODE_POSITION_CLOSED
  | TRADE_RETCODE_INVALID_CLOSE_VOLUME
  | TRADE_RETCODE_CLOSE_ORDER_EXIST
  | TRADE_RETCODE_LIMIT_POSITIONS
  | TRADE_RETCODE_REJECT_CANCEL
  | TRADE_RETCODE_LONG_ONLY
  | TRADE_RETCODE_SHORT_ONLY
  | TRADE_RETCODE_CLOSE_ONLY
  | TRADE_RETCODE_FIFO_CLOSE
  | TRADE_RETCODE_HEDGE_PROHIBITED
  | TRADE_RETCODE_UNKNOWN
  deriving (Show, Eq, Ord, Enum)

toTradeRetcode :: Int -> TradeRetcode
toTradeRetcode x =
  case x of
    10004 -> TRADE_RETCODE_REQUOTE
    10006 -> TRADE_RETCODE_REJECT
    10007 -> TRADE_RETCODE_CANCEL
    10008 -> TRADE_RETCODE_PLACED
    10009 -> TRADE_RETCODE_DONE
    10010 -> TRADE_RETCODE_DONE_PARTIAL
    10011 -> TRADE_RETCODE_ERROR
    10012 -> TRADE_RETCODE_TIMEOUT
    10013 -> TRADE_RETCODE_INVALID
    10014 -> TRADE_RETCODE_INVALID_VOLUME
    10015 -> TRADE_RETCODE_INVALID_PRICE
    10016 -> TRADE_RETCODE_INVALID_STOPS
    10017 -> TRADE_RETCODE_TRADE_DISABLED
    10018 -> TRADE_RETCODE_MARKET_CLOSED
    10019 -> TRADE_RETCODE_NO_MONEY
    10020 -> TRADE_RETCODE_PRICE_CHANGED
    10021 -> TRADE_RETCODE_PRICE_OFF
    10022 -> TRADE_RETCODE_INVALID_EXPIRATION
    10023 -> TRADE_RETCODE_ORDER_CHANGED
    10024 -> TRADE_RETCODE_TOO_MANY_REQUESTS
    10025 -> TRADE_RETCODE_NO_CHANGES
    10026 -> TRADE_RETCODE_SERVER_DISABLES_AT
    10027 -> TRADE_RETCODE_CLIENT_DISABLES_AT
    10028 -> TRADE_RETCODE_LOCKED
    10029 -> TRADE_RETCODE_FROZEN
    10030 -> TRADE_RETCODE_INVALID_FILL
    10031 -> TRADE_RETCODE_CONNECTION
    10032 -> TRADE_RETCODE_ONLY_REAL
    10033 -> TRADE_RETCODE_LIMIT_ORDERS
    10034 -> TRADE_RETCODE_LIMIT_VOLUME
    10035 -> TRADE_RETCODE_INVALID_ORDER
    10036 -> TRADE_RETCODE_POSITION_CLOSED
    10038 -> TRADE_RETCODE_INVALID_CLOSE_VOLUME
    10039 -> TRADE_RETCODE_CLOSE_ORDER_EXIST
    10040 -> TRADE_RETCODE_LIMIT_POSITIONS
    10041 -> TRADE_RETCODE_REJECT_CANCEL
    10042 -> TRADE_RETCODE_LONG_ONLY
    10043 -> TRADE_RETCODE_SHORT_ONLY
    10044 -> TRADE_RETCODE_CLOSE_ONLY
    10045 -> TRADE_RETCODE_FIFO_CLOSE
    10046 -> TRADE_RETCODE_HEDGE_PROHIBITED
    _     -> TRADE_RETCODE_UNKNOWN

-- | Return code definitions
tradeRetcodeNumber :: TradeRetcode -> Int
tradeRetcodeNumber x =
  case x of
    TRADE_RETCODE_REQUOTE              -> 10004
    TRADE_RETCODE_REJECT               -> 10006
    TRADE_RETCODE_CANCEL               -> 10007
    TRADE_RETCODE_PLACED               -> 10008
    TRADE_RETCODE_DONE                 -> 10009
    TRADE_RETCODE_DONE_PARTIAL         -> 10010
    TRADE_RETCODE_ERROR                -> 10011
    TRADE_RETCODE_TIMEOUT              -> 10012
    TRADE_RETCODE_INVALID              -> 10013
    TRADE_RETCODE_INVALID_VOLUME       -> 10014
    TRADE_RETCODE_INVALID_PRICE        -> 10015
    TRADE_RETCODE_INVALID_STOPS        -> 10016
    TRADE_RETCODE_TRADE_DISABLED       -> 10017
    TRADE_RETCODE_MARKET_CLOSED        -> 10018
    TRADE_RETCODE_NO_MONEY             -> 10019
    TRADE_RETCODE_PRICE_CHANGED        -> 10020
    TRADE_RETCODE_PRICE_OFF            -> 10021
    TRADE_RETCODE_INVALID_EXPIRATION   -> 10022
    TRADE_RETCODE_ORDER_CHANGED        -> 10023
    TRADE_RETCODE_TOO_MANY_REQUESTS    -> 10024
    TRADE_RETCODE_NO_CHANGES           -> 10025
    TRADE_RETCODE_SERVER_DISABLES_AT   -> 10026
    TRADE_RETCODE_CLIENT_DISABLES_AT   -> 10027
    TRADE_RETCODE_LOCKED               -> 10028
    TRADE_RETCODE_FROZEN               -> 10029
    TRADE_RETCODE_INVALID_FILL         -> 10030
    TRADE_RETCODE_CONNECTION           -> 10031
    TRADE_RETCODE_ONLY_REAL            -> 10032
    TRADE_RETCODE_LIMIT_ORDERS         -> 10033
    TRADE_RETCODE_LIMIT_VOLUME         -> 10034
    TRADE_RETCODE_INVALID_ORDER        -> 10035
    TRADE_RETCODE_POSITION_CLOSED      -> 10036
    TRADE_RETCODE_INVALID_CLOSE_VOLUME -> 10038
    TRADE_RETCODE_CLOSE_ORDER_EXIST    -> 10039
    TRADE_RETCODE_LIMIT_POSITIONS      -> 10040
    TRADE_RETCODE_REJECT_CANCEL        -> 10041
    TRADE_RETCODE_LONG_ONLY            -> 10042
    TRADE_RETCODE_SHORT_ONLY           -> 10043
    TRADE_RETCODE_CLOSE_ONLY           -> 10044
    TRADE_RETCODE_FIFO_CLOSE           -> 10045
    TRADE_RETCODE_HEDGE_PROHIBITED     -> 10046
    _                                  -> -1


-- | Return code definitions
explainTradeRetcodes :: TradeRetcode -> String
explainTradeRetcodes x =
  case x of
    TRADE_RETCODE_REQUOTE                  -> "Requote"
    TRADE_RETCODE_REJECT                   -> "Request rejected"
    TRADE_RETCODE_CANCEL                   -> "Request canceled by trader"
    TRADE_RETCODE_PLACED                   -> "Order placed"
    TRADE_RETCODE_DONE                     -> "Request completed"
    TRADE_RETCODE_DONE_PARTIAL             -> "Only part of the request was completed"
    TRADE_RETCODE_ERROR                    -> "Request processing error"
    TRADE_RETCODE_TIMEOUT                  -> "Request canceled by timeout"
    TRADE_RETCODE_INVALID                  -> "Invalid request"
    TRADE_RETCODE_INVALID_VOLUME           -> "Invalid volume in the request"
    TRADE_RETCODE_INVALID_PRICE            -> "Invalid price in the request"
    TRADE_RETCODE_INVALID_STOPS            -> "Invalid stops in the request"
    TRADE_RETCODE_TRADE_DISABLED           -> "Trade is disabled"
    TRADE_RETCODE_MARKET_CLOSED            -> "Market is closed"
    TRADE_RETCODE_NO_MONEY                 -> "There is not enough money to complete the request"
    TRADE_RETCODE_PRICE_CHANGED            -> "Prices changed"
    TRADE_RETCODE_PRICE_OFF                -> "There are no quotes to process the request"
    TRADE_RETCODE_INVALID_EXPIRATION       -> "Invalid order expiration date in the request"
    TRADE_RETCODE_ORDER_CHANGED            -> "Order state changed"
    TRADE_RETCODE_TOO_MANY_REQUESTS        -> "Too frequent requests"
    TRADE_RETCODE_NO_CHANGES               -> "No changes in request"
    TRADE_RETCODE_SERVER_DISABLES_AT       -> "Autotrading disabled by server"
    TRADE_RETCODE_CLIENT_DISABLES_AT       -> "Autotrading disabled by client terminal"
    TRADE_RETCODE_LOCKED                   -> "Request locked for processing"
    TRADE_RETCODE_FROZEN                   -> "Order or position frozen"
    TRADE_RETCODE_INVALID_FILL             -> "Invalid order filling type"
    TRADE_RETCODE_CONNECTION               -> "No connection with the trade server"
    TRADE_RETCODE_ONLY_REAL                -> "Operation is allowed only for live accounts"
    TRADE_RETCODE_LIMIT_ORDERS             -> "The number of pending orders has reached the limit"
    TRADE_RETCODE_LIMIT_VOLUME             -> "The volume of orders and positions for the symbol has reached the limit"
    TRADE_RETCODE_INVALID_ORDER            -> "Incorrect or prohibited order type"
    TRADE_RETCODE_POSITION_CLOSED          -> "Position with the specified POSITION_IDENTIFIER has already been closed"
    TRADE_RETCODE_INVALID_CLOSE_VOLUME     -> "A close volume exceeds the current position volume"
    TRADE_RETCODE_CLOSE_ORDER_EXIST        -> "A close order already exists for a specified position. This may happen when working in the hedging system: i) when attempting to close a position with an opposite one, while close orders for the position already exist; ii) when attempting to fully or partially close a position if the total volume of the already present close orders and the newly placed one exceeds the current position volume; "
    TRADE_RETCODE_LIMIT_POSITIONS          -> "The number of open positions simultaneously present on an account can be limited by the server settings. After a limit is reached, the server returns the TRADE_RETCODE_LIMIT_POSITIONS error when attempting to place an order. The limitation operates differently depending on the position accounting type: Netting — number of open positions is considered. When a limit is reached, the platform does not let placing new orders whose execution may increase the number of open positions. In fact, the platform allows placing orders only for the symbols that already have open positions. The current pending orders are not considered since their execution may lead to changes in the current positions but it cannot increase their number. Hedging — pending orders are considered together with open positions, since a pending order activation always leads to opening a new position. When a limit is reached, the platform does not allow placing both new market orders for opening positions and pending orders."
    TRADE_RETCODE_REJECT_CANCEL            -> "The pending order activation request is rejected, the order is canceled"
    TRADE_RETCODE_LONG_ONLY                -> "The request is rejected, because the 'Only long positions are allowed' rule is set for the symbol (POSITION_TYPE_BUY)"
    TRADE_RETCODE_SHORT_ONLY               -> "The request is rejected, because the 'Only short positions are allowed' rule is set for the symbol (POSITION_TYPE_SELL)"
    TRADE_RETCODE_CLOSE_ONLY               -> "The request is rejected, because the 'Only position closing is allowed' rule is set for the symbol"
    TRADE_RETCODE_FIFO_CLOSE               -> "The request is rejected, because 'Position closing is allowed only by FIFO rule' flag is set for the trading account (ACCOUNT_FIFO_CLOSE=true)"
    TRADE_RETCODE_HEDGE_PROHIBITED         -> "The request is rejected, because the 'Opposite positions on a single symbol are disabled' rule is set for the trading account. For example, if the account has a Buy position, then a user cannot open a Sell position or place a pending sell order. The rule is only applied to accounts with hedging accounting system (ACCOUNT_MARGIN_MODE=ACCOUNT_MARGIN_MODE_RETAIL_HEDGING)."
    _                                      -> "Unknown"
