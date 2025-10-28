module MT5.Data.MqlTradeRequest
  ( MqlTradeRequest(..)
  ) where

import           MT5.Data.OrderType
import           MT5.Data.OrderTypeFilling
import           MT5.Data.OrderTypeTime
import           MT5.Data.TradeRequestAction
import           MT5.Data.DecimalNumber

data MqlTradeRequest = MqlTradeRequest
  { trReqAction      :: TradeRequestAction -- ^ Trading operation type. The value can be one of the values of the
                                           -- TRADE_REQUEST_ACTIONS enumeration
  , trReqMagic       :: Int                -- ^ EA ID. Allows arranging the analytical handling of trading orders.
                                           -- Each EA can set a unique ID when sending a trading request
  , trReqOrder       :: Integer            -- ^ Order ticket. Required for modifying pending orders
  , trReqSymbol      :: String             -- ^ The name of the trading instrument, for which the order is placed. Not
                                           -- required when modifying orders and closing positions
  , trReqVolume      :: DecimalNumber      -- ^ Requested volume of a deal in lots. A real volume when making a deal
                                           -- depends on an order execution type.
  , trReqPrice       :: DecimalNumber      -- ^ Price at which an order should be executed. The price is not set in
                                           -- case of market orders for instruments of the "Market Execution"
                                           -- (SYMBOL_TRADE_EXECUTION_MARKET) type having the TRADE_ACTION_DEAL type
  , trReqStoplimit   :: DecimalNumber      -- ^ A price a pending Limit order is set at when the price reaches the
                                           -- 'price' value (this condition is mandatory). The pending order is not
                                           -- passed to the trading system until that moment
  , trReqSl          :: DecimalNumber      -- ^ A price a Stop Loss order is activated at when the price moves in an
                                           -- unfavorable direction
  , trReqTp          :: DecimalNumber      -- ^ A price a Take Profit order is activated at when the price moves in a
                                           -- favorable direction
  , trReqDeviation   :: Int                -- ^ Maximum acceptable deviation from the requested price, specified in
                                           -- points
  , trReqType        :: OrderType          -- ^ Order type. The value can be one of the values of the ORDER_TYPE
                                           -- enumeration
  , trReqTypeFilling :: OrderTypeFilling   -- ^ Order filling type. The value can be one of the ORDER_TYPE_FILLING
                                           -- values
  , trReqTypeTime    :: OrderTypeTime      -- ^ Order type by expiration. The value can be one of the ORDER_TYPE_TIME
                                           -- values
  , trReqExpiration  :: Int                -- ^ Pending order expiration time (for TIME_SPECIFIED type orders)
  , trReqComment     :: String             -- ^ Comment to an order
  , trReqPosition    :: Integer            -- ^ Position ticket. Fill it when changing and closing a position for its
                                           -- clear identification. Usually, it is the same as the ticket of the order
                                           -- that opened the position.
  , trReqPositionBy  :: Integer            -- ^ Opposite position ticket. It is used when closing a position by an
                                           -- opposite one (opened at the same symbol but in the opposite direction).
  }
  deriving (Show)
