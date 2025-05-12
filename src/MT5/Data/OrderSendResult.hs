module MT5.Data.OrderSendResult
    ( OrderSendResult(..)
    , readOrderSendResult
    , ordSendRetcodes
    , ordSendRetcodeName
    , ordSendRetcodeExplain
    ) where

import           MT5.Communication

data OrderSendResult = OrderSendResult
  { ordSendRetcode          :: Int -- ^ See `ordSendRetcodes`, `ordSendRetcodeExplain` and `ordSendRetcodeName`.
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

-- OrderSendResult {ordSendRetcode = 10009, ordSendDeal = 0, ordSendOrder = 93681576, ordSendVolume = 1.0e-2,
-- ordSendPrice = 0.0, ordSendBid = 0.0, ordSendAsk = 0.0, ordSendComment = "Request executed", ordSendRequest_id =
-- 4259310475, ordSendRetcode_external = 0}


readOrderSendResult :: IO OrderSendResult
readOrderSendResult =
  OrderSendResult
  <$> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)

-- | Return code variable name.
ordSendRetcodeName :: Int -> String
ordSendRetcodeName = fst . ordSendRetcodes

-- | Textual explanation of return code.
ordSendRetcodeExplain :: Int -> String
ordSendRetcodeExplain = snd . ordSendRetcodes

-- | Return code definitions
ordSendRetcodes :: Int -> (String, String)
ordSendRetcodes x =
  case x of
    10004 -> ("TRADE_RETCODE_REQUOTE", " Requote")
    10006 -> ("TRADE_RETCODE_REJECT", " Request rejected")
    10007 -> ("TRADE_RETCODE_CANCEL", "Request canceled by trader")
    10008 -> ("TRADE_RETCODE_PLACED", "Order placed")
    10009 -> ("TRADE_RETCODE_DONE", "Request completed")
    10010 -> ("TRADE_RETCODE_DONE_PARTIAL", "Only part of the request was completed")
    10011 -> ("TRADE_RETCODE_ERROR", "Request processing error")
    10012 -> ("TRADE_RETCODE_TIMEOUT", "Request canceled by timeout")
    10013 -> ("TRADE_RETCODE_INVALID", "Invalid request")
    10014 -> ("TRADE_RETCODE_INVALID_VOLUME", "Invalid volume in the request")
    10015 -> ("TRADE_RETCODE_INVALID_PRICE", "Invalid price in the request")
    10016 -> ("TRADE_RETCODE_INVALID_STOPS", "Invalid stops in the request")
    10017 -> ("TRADE_RETCODE_TRADE_DISABLED", "Trade is disabled")
    10018 -> ("TRADE_RETCODE_MARKET_CLOSED", "Market is closed")
    10019 -> ("TRADE_RETCODE_NO_MONEY", "There is not enough money to complete the request")
    10020 -> ("TRADE_RETCODE_PRICE_CHANGED", "Prices changed")
    10021 -> ("TRADE_RETCODE_PRICE_OFF", "There are no quotes to process the request")
    10022 -> ("TRADE_RETCODE_INVALID_EXPIRATION", "Invalid order expiration date in the request")
    10023 -> ("TRADE_RETCODE_ORDER_CHANGED", "Order state changed")
    10024 -> ("TRADE_RETCODE_TOO_MANY_REQUESTS", "Too frequent requests")
    10025 -> ("TRADE_RETCODE_NO_CHANGES", "No changes in request")
    10026 -> ("TRADE_RETCODE_SERVER_DISABLES_AT", "Autotrading disabled by server")
    10027 -> ("TRADE_RETCODE_CLIENT_DISABLES_AT", "Autotrading disabled by client terminal")
    10028 -> ("TRADE_RETCODE_LOCKED", "Request locked for processing")
    10029 -> ("TRADE_RETCODE_FROZEN", "Order or position frozen")
    10030 -> ("TRADE_RETCODE_INVALID_FILL", "Invalid order filling type")
    10031 -> ("TRADE_RETCODE_CONNECTION", "No connection with the trade server")
    10032 -> ("TRADE_RETCODE_ONLY_REAL", "Operation is allowed only for live accounts")
    10033 -> ("TRADE_RETCODE_LIMIT_ORDERS", "The number of pending orders has reached the limit")
    10034 -> ("TRADE_RETCODE_LIMIT_VOLUME", "The volume of orders and positions for the symbol has reached the limit")
    10035 -> ("TRADE_RETCODE_INVALID_ORDER", "Incorrect or prohibited order type")
    10036 ->
      ("TRADE_RETCODE_POSITION_CLOSED", "Position with the specified POSITION_IDENTIFIER has already been closed")
    10038 -> ("TRADE_RETCODE_INVALID_CLOSE_VOLUME", "A close volume exceeds the current position volume")
    10039 ->
      ( "TRADE_RETCODE_CLOSE_ORDER_EXIST"
      , "A close order already exists for a specified position. This may happen when working in the hedging system: i) when attempting to close a position with an opposite one, while close orders for the position already exist; ii) when attempting to fully or partially close a position if the total volume of the already present close orders and the newly placed one exceeds the current position volume; ")
    10040 ->
      ( "TRADE_RETCODE_LIMIT_POSITIONS"
      , "The number of open positions simultaneously present on an account can be limited by the server settings. After a limit is reached, the server returns the TRADE_RETCODE_LIMIT_POSITIONS error when attempting to place an order. The limitation operates differently depending on the position accounting type: Netting — number of open positions is considered. When a limit is reached, the platform does not let placing new orders whose execution may increase the number of open positions. In fact, the platform allows placing orders only for the symbols that already have open positions. The current pending orders are not considered since their execution may lead to changes in the current positions but it cannot increase their number. Hedging — pending orders are considered together with open positions, since a pending order activation always leads to opening a new position. When a limit is reached, the platform does not allow placing both new market orders for opening positions and pending orders.")
    10041 -> ("TRADE_RETCODE_REJECT_CANCEL", "The pending order activation request is rejected, the order is canceled")
    10042 ->
      ( "TRADE_RETCODE_LONG_ONLY"
      , "The request is rejected, because the 'Only long positions are allowed' rule is set for the symbol (POSITION_TYPE_BUY)")
    10043 ->
      ( "TRADE_RETCODE_SHORT_ONLY"
      , "The request is rejected, because the 'Only short positions are allowed' rule is set for the symbol (POSITION_TYPE_SELL)")
    10044 ->
      ( "TRADE_RETCODE_CLOSE_ONLY"
      , "The request is rejected, because the 'Only position closing is allowed' rule is set for the symbol")
    10045 ->
      ( "TRADE_RETCODE_FIFO_CLOSE"
      , "The request is rejected, because 'Position closing is allowed only by FIFO rule' flag is set for the trading account (ACCOUNT_FIFO_CLOSE=true)")
    10046 ->
      ( "TRADE_RETCODE_HEDGE_PROHIBITED"
      , "The request is rejected, because the 'Opposite positions on a single symbol are disabled' rule is set for the trading account. For example, if the account has a Buy position, then a user cannot open a Sell position or place a pending sell order. The rule is only applied to accounts with hedging accounting system (ACCOUNT_MARGIN_MODE=ACCOUNT_MARGIN_MODE_RETAIL_HEDGING).")
    _ -> ("Unknown", "Unknown")
