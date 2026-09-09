{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Type-safe response types for MT5 file-based communication
--
-- This module provides domain-specific response types with FromJSON instances
-- for deserializing responses from the MT5 EA. Includes comprehensive error
-- handling and validation.
module MT5.Communication.Response
    ( -- * Order Management Responses
      OrderSendResponse(..)
    , PositionCloseResponse(..)
    , PositionModifyResponse(..)
      -- * Symbol Information Responses
    , SymbolInfoResponse(..)
    , SymbolSelectResponse(..)
    , SymbolsGetResponse(..)
      -- * Historical Data Responses
    , CandlesGetResponse(..)
    , OHLCVCandle(..)
      -- * Account & Position Query Responses
    , AccountInfoResponse(..)
    , PositionsGetResponse(..)
    , PositionInfoResponse(..)
    , OrdersGetResponse(..)
    , OrderInfoResponse(..)
      -- * Response Parsing
    , parseResponse
    , ResponseError(..)
    ) where

import           Control.Applicative ((<|>))
import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON (..), Value (..), withObject,
                                      (.!=), (.:), (.:?))
import           Data.Aeson.Types    (Parser, parseEither)
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)
import           GHC.Generics        (Generic)

import           MT5.Data.DecimalNumber


-- | Response parsing errors
data ResponseError
  = ParseFailed String        -- ^ JSON parsing failed
  | MT5Error Int Text         -- ^ MT5 returned error code and message
  | UnexpectedFormat Text     -- ^ Response format was unexpected
  deriving (Show, Eq, Generic, NFData)


--------------------------------------------------------------------------------
-- Helper: Parse response with error handling
--------------------------------------------------------------------------------

-- | Parse a Value into a response type, handling MT5 errors
parseResponse :: FromJSON a => Value -> Either ResponseError a
parseResponse val = case parseEither parseJSON val of
  Left err  -> Left (ParseFailed err)
  Right res -> Right res


--------------------------------------------------------------------------------
-- Order Management Responses
--------------------------------------------------------------------------------

-- | Response from order_send action
data OrderSendResponse = OrderSendResponse
  { orderSendRespSuccess :: !Bool
  , orderSendRespRetcode :: !Int
  , orderSendRespDeal    :: !Int
  , orderSendRespOrder   :: !Int
  , orderSendRespVolume  :: !DecimalNumber
  , orderSendRespPrice   :: !Double
  , orderSendRespComment :: !Text
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON OrderSendResponse where
  parseJSON = withObject "OrderSendResponse" $ \o -> do
    success <- o .: "success"
    retcode <- o .: "retcode"
    comment <- o .:? "comment" .!= ""
    if success
      then OrderSendResponse success retcode
             <$> o .: "deal"
             <*> o .: "order"
             <*> o .: "volume"
             <*> o .: "price"
             <*> pure comment
      else return $ OrderSendResponse success retcode 0 0 (DecimalNumber Nothing 0.0) 0.0 comment


-- | Response from position_close and position_close_partial
data PositionCloseResponse = PositionCloseResponse
  { positionCloseSuccess :: !Bool
  , positionCloseRetcode :: !Int
  , positionCloseDeal    :: !Int
  , positionCloseOrder   :: !(Maybe Int)
  , positionCloseVolume  :: !(Maybe DecimalNumber)
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON PositionCloseResponse where
  parseJSON = withObject "PositionCloseResponse" $ \o ->
    PositionCloseResponse
      <$> o .: "success"
      <*> o .: "retcode"
      <*> o .: "deal"
      <*> o .:? "order"
      <*> o .:? "volume"


-- | Response from position_modify
data PositionModifyResponse = PositionModifyResponse
  { positionModifySuccess :: !Bool
  , positionModifyRetcode :: !Int
  , positionModifySl      :: !(Maybe Double)
  , positionModifyTp      :: !(Maybe Double)
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON PositionModifyResponse where
  parseJSON = withObject "PositionModifyResponse" $ \o ->
    PositionModifyResponse
      <$> o .: "success"
      <*> o .: "retcode"
      <*> o .:? "sl"
      <*> o .:? "tp"


--------------------------------------------------------------------------------
-- Symbol Information Responses
--------------------------------------------------------------------------------

-- | Response from symbol_info action
data SymbolInfoResponse = SymbolInfoResponse
  { symbolInfoSuccess :: !Bool
  , symbolInfoSymbol  :: !Text
  , symbolInfoBid     :: !Double
  , symbolInfoAsk     :: !Double
  , symbolInfoDigits  :: !Int
  , symbolInfoSpread  :: !Int
  , symbolInfoPoint   :: !Double
  , symbolInfoContractSize      :: !(Maybe Double)  -- ^ SYMBOL_TRADE_CONTRACT_SIZE, when the EA reports it.
  , symbolInfoVolumeMin         :: !(Maybe Double)  -- ^ SYMBOL_VOLUME_MIN, minimum tradable volume.
  , symbolInfoVolumeMax         :: !(Maybe Double)  -- ^ SYMBOL_VOLUME_MAX, maximum tradable volume.
  , symbolInfoVolumeStep        :: !(Maybe Double)  -- ^ SYMBOL_VOLUME_STEP, volume increment (drives volume precision).
  , symbolInfoVolumeLimit       :: !(Maybe Double)  -- ^ SYMBOL_VOLUME_LIMIT, aggregate volume cap.
  , symbolInfoTradeCalcMode     :: !(Maybe Int)     -- ^ SYMBOL_TRADE_CALC_MODE, drives instrument-type classification.
  , symbolInfoTradeMode         :: !(Maybe Int)     -- ^ SYMBOL_TRADE_MODE, trading permission.
  , symbolInfoTradeExemode      :: !(Maybe Int)     -- ^ SYMBOL_TRADE_EXEMODE, execution mode.
  , symbolInfoTradeStopsLevel   :: !(Maybe Int)     -- ^ SYMBOL_TRADE_STOPS_LEVEL, min SL/TP distance (points).
  , symbolInfoTradeFreezeLevel  :: !(Maybe Int)     -- ^ SYMBOL_TRADE_FREEZE_LEVEL, freeze distance (points).
  , symbolInfoFillingMode       :: !(Maybe Int)     -- ^ SYMBOL_FILLING_MODE, allowed fill policies bitmask.
  , symbolInfoExpirationMode    :: !(Maybe Int)     -- ^ SYMBOL_EXPIRATION_MODE, allowed expiration types bitmask.
  , symbolInfoTickValue         :: !(Maybe Double)  -- ^ SYMBOL_TRADE_TICK_VALUE, account-currency value per tick.
  , symbolInfoTickValueProfit   :: !(Maybe Double)  -- ^ SYMBOL_TRADE_TICK_VALUE_PROFIT.
  , symbolInfoTickValueLoss     :: !(Maybe Double)  -- ^ SYMBOL_TRADE_TICK_VALUE_LOSS.
  , symbolInfoTickSize          :: !(Maybe Double)  -- ^ SYMBOL_TRADE_TICK_SIZE, minimal price change.
  , symbolInfoMarginInitial     :: !(Maybe Double)  -- ^ SYMBOL_MARGIN_INITIAL, drives margin-rate calc.
  , symbolInfoMarginMaintenance :: !(Maybe Double)  -- ^ SYMBOL_MARGIN_MAINTENANCE.
  , symbolInfoSwapLong          :: !(Maybe Double)  -- ^ SYMBOL_SWAP_LONG.
  , symbolInfoSwapShort         :: !(Maybe Double)  -- ^ SYMBOL_SWAP_SHORT.
  , symbolInfoCurrencyBase      :: !(Maybe Text)    -- ^ SYMBOL_CURRENCY_BASE.
  , symbolInfoCurrencyProfit    :: !(Maybe Text)    -- ^ SYMBOL_CURRENCY_PROFIT.
  , symbolInfoCurrencyMargin    :: !(Maybe Text)    -- ^ SYMBOL_CURRENCY_MARGIN.
  , symbolInfoDescription       :: !(Maybe Text)    -- ^ SYMBOL_DESCRIPTION.
  , symbolInfoPath              :: !(Maybe Text)    -- ^ SYMBOL_PATH.
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON SymbolInfoResponse where
  parseJSON = withObject "SymbolInfoResponse" $ \o ->
    SymbolInfoResponse
      <$> o .: "success"
      <*> o .: "symbol"
      <*> o .: "bid"
      <*> o .: "ask"
      <*> o .: "digits"
      <*> o .: "spread"
      <*> o .: "point"
      <*> o .:? "trade_contract_size"
      <*> o .:? "volume_min"
      <*> o .:? "volume_max"
      <*> o .:? "volume_step"
      <*> o .:? "volume_limit"
      <*> o .:? "trade_calc_mode"
      <*> o .:? "trade_mode"
      <*> o .:? "trade_exemode"
      <*> o .:? "trade_stops_level"
      <*> o .:? "trade_freeze_level"
      <*> o .:? "filling_mode"
      <*> o .:? "expiration_mode"
      <*> o .:? "tick_value"
      <*> o .:? "tick_value_profit"
      <*> o .:? "tick_value_loss"
      <*> o .:? "tick_size"
      <*> o .:? "margin_initial"
      <*> o .:? "margin_maintenance"
      <*> o .:? "swap_long"
      <*> o .:? "swap_short"
      <*> o .:? "currency_base"
      <*> o .:? "currency_profit"
      <*> o .:? "currency_margin"
      <*> o .:? "description"
      <*> o .:? "path"


-- | Response from symbol_select action
data SymbolSelectResponse = SymbolSelectResponse
  { symbolSelectSuccess  :: !Bool
  , symbolSelectSymbol   :: !Text
  , symbolSelectSelected :: !Bool
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON SymbolSelectResponse where
  parseJSON = withObject "SymbolSelectResponse" $ \o ->
    SymbolSelectResponse
      <$> o .: "success"
      <*> o .: "symbol"
      <*> o .: "selected"


-- | Response from symbols_get action
data SymbolsGetResponse = SymbolsGetResponse
  { symbolsGetSuccess :: !Bool
  , symbolsGetCount   :: !Int
  , symbolsGetSymbols :: ![Text]
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON SymbolsGetResponse where
  parseJSON = withObject "SymbolsGetResponse" $ \o ->
    SymbolsGetResponse
      <$> o .: "success"
      <*> o .: "count"
      <*> o .: "symbols"


--------------------------------------------------------------------------------
-- Historical Data Responses
--------------------------------------------------------------------------------

-- | Single OHLCV candle from EA
data OHLCVCandle = OHLCVCandle
  { candleTime       :: !Int     -- ^ Unix timestamp
  , candleOpen       :: !Double
  , candleHigh       :: !Double
  , candleLow        :: !Double
  , candleClose      :: !Double
  , candleVolume     :: !Int     -- ^ Tick volume
  , candleSpread     :: !Int     -- ^ Spread in points
  , candleRealVolume :: !Double  -- ^ Real volume
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON OHLCVCandle where
  parseJSON = withObject "OHLCVCandle" $ \o ->
    OHLCVCandle
      <$> o .: "time"
      <*> o .: "open"
      <*> o .: "high"
      <*> o .: "low"
      <*> o .: "close"
      <*> o .: "volume"
      <*> o .:? "spread" .!= 0
      <*> o .:? "real_volume" .!= 0


-- | Response from candles_get action
data CandlesGetResponse = CandlesGetResponse
  { candlesGetSuccess   :: !Bool
  , candlesGetSymbol    :: !Text
  , candlesGetTimeframe :: !Text
  , candlesGetCount     :: !Int
  , candlesGetCandles   :: ![OHLCVCandle]
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON CandlesGetResponse where
  parseJSON = withObject "CandlesGetResponse" $ \o ->
    CandlesGetResponse
      <$> o .: "success"
      <*> o .: "symbol"
      <*> o .: "timeframe"
      <*> o .: "count"
      <*> o .: "candles"


--------------------------------------------------------------------------------
-- Account & Position Query Responses
--------------------------------------------------------------------------------

-- | Response from account_info action
data AccountInfoResponse = AccountInfoResponse
  { accountInfoSuccess      :: !Bool
  , accountInfoLogin        :: !Int
  , accountInfoBalance      :: !Double
  , accountInfoEquity       :: !Double
  , accountInfoProfit       :: !Double
  , accountInfoMargin       :: !Double
  , accountInfoMarginFree   :: !Double
  , accountInfoMarginLevel  :: !Double
  , accountInfoLeverage     :: !Int
  , accountInfoCurrency     :: !Text
  , accountInfoName         :: !Text
  , accountInfoServer       :: !Text
  , accountInfoTradeAllowed :: !Bool
  , accountInfoTradeExpert  :: !Bool
  , accountInfoTradeMode         :: !(Maybe Int)     -- ^ ACCOUNT_TRADE_MODE (demo/contest/real).
  , accountInfoMarginMode        :: !(Maybe Int)     -- ^ ACCOUNT_MARGIN_MODE (retail netting/hedging/exchange).
  , accountInfoCurrencyDigits    :: !(Maybe Int)     -- ^ ACCOUNT_CURRENCY_DIGITS.
  , accountInfoLimitOrders       :: !(Maybe Int)     -- ^ ACCOUNT_LIMIT_ORDERS.
  , accountInfoFifoClose         :: !(Maybe Bool)    -- ^ ACCOUNT_FIFO_CLOSE.
  , accountInfoCredit            :: !(Maybe Double)  -- ^ ACCOUNT_CREDIT.
  , accountInfoMarginSoCall      :: !(Maybe Double)  -- ^ ACCOUNT_MARGIN_SO_CALL (margin-call level).
  , accountInfoMarginSoSo        :: !(Maybe Double)  -- ^ ACCOUNT_MARGIN_SO_SO (stop-out level).
  , accountInfoMarginInitial     :: !(Maybe Double)  -- ^ ACCOUNT_MARGIN_INITIAL.
  , accountInfoMarginMaintenance :: !(Maybe Double)  -- ^ ACCOUNT_MARGIN_MAINTENANCE.
  , accountInfoCompany           :: !(Maybe Text)    -- ^ ACCOUNT_COMPANY.
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON AccountInfoResponse where
  parseJSON = withObject "AccountInfoResponse" $ \o ->
    AccountInfoResponse
      <$> o .: "success"
      <*> o .: "login"
      <*> o .: "balance"
      <*> o .: "equity"
      <*> o .: "profit"
      <*> o .: "margin"
      <*> o .: "margin_free"
      <*> o .: "margin_level"
      <*> o .: "leverage"
      <*> o .: "currency"
      <*> o .: "name"
      <*> o .: "server"
      <*> o .: "trade_allowed"
      <*> o .: "trade_expert"
      <*> o .:? "trade_mode"
      <*> o .:? "margin_mode"
      <*> o .:? "currency_digits"
      <*> o .:? "limit_orders"
      <*> o .:? "fifo_close"
      <*> o .:? "credit"
      <*> o .:? "margin_so_call"
      <*> o .:? "margin_so_so"
      <*> o .:? "margin_initial"
      <*> o .:? "margin_maintenance"
      <*> o .:? "company"


-- | Single position info from positions_get
-- {"ticket":128328176,"symbol":"EURUSD.pro","type":0,"volume":0.04000000,"price_open":1.16547000,"price_current":1.16543000,"sl":1.15273000,"tp":0.00000000,"profit":-0.14000000,"swap":0.00000000,"commission":0.00000000,"magic":0,"comment":"aral-trader market order"}
data PositionInfoResponse = PositionInfoResponse
  { positionTicket       :: !Integer
  , positionSymbol       :: !Text
  , positionType         :: !Int         -- ^ 0 = BUY, 1 = SELL
  , positionVolume       :: !DecimalNumber
  , positionPriceOpen    :: !Double
  , positionPriceCurrent :: !Double
  , positionSl           :: !Double
  , positionTp           :: !Double
  , positionProfit       :: !Double
  , positionSwap         :: !Double
  , positionMagic        :: !Int
  , positionComment      :: !Text
  , positionIdentifier   :: !(Maybe Integer)  -- ^ POSITION_IDENTIFIER, stable position id across partial fills.
  , positionTime         :: !(Maybe Integer)  -- ^ POSITION_TIME (server seconds), open time.
  , positionTimeUpdate   :: !(Maybe Integer)  -- ^ POSITION_TIME_UPDATE (server seconds), last SL/TP change.
  , positionTimeMsc      :: !(Maybe Integer)  -- ^ POSITION_TIME_MSC (server milliseconds), open time.
  , positionTimeUpdateMsc :: !(Maybe Integer) -- ^ POSITION_TIME_UPDATE_MSC (server milliseconds).
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON PositionInfoResponse where
  parseJSON = withObject "PositionInfoResponse" $ \o ->
    PositionInfoResponse
      <$> o .: "ticket"
      <*> o .: "symbol"
      <*> o .: "type"
      <*> o .: "volume"
      <*> o .: "price_open"
      <*> o .: "price_current"
      <*> o .: "sl"
      <*> o .: "tp"
      <*> o .: "profit"
      <*> o .: "swap"
      <*> o .: "magic"
      <*> o .:? "comment" .!= ""
      <*> o .:? "identifier"
      <*> o .:? "time"
      <*> o .:? "time_update"
      <*> o .:? "time_msc"
      <*> o .:? "time_update_msc"


-- | Response from positions_get action
data PositionsGetResponse = PositionsGetResponse
  { positionsGetSuccess   :: !Bool
  , positionsGetCount     :: !Int
  , positionsGetPositions :: ![PositionInfoResponse]
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON PositionsGetResponse where
  parseJSON = withObject "PositionsGetResponse" $ \o ->
    PositionsGetResponse
      <$> o .: "success"
      <*> o .: "count"
      <*> (o .: "" <|> o .: "positions")  -- MQL5 EA uses empty string key


-- | Single order info from orders_get
data OrderInfoResponse = OrderInfoResponse
  { orderTicket       :: !Int
  , orderSymbol       :: !Text
  , orderType         :: !Int         -- ^ Order type enum
  , orderVolume       :: !DecimalNumber
  , orderPriceOpen    :: !Double
  , orderPriceCurrent :: !Double
  , orderSl           :: !Double
  , orderTp           :: !Double
  , orderMagic        :: !Int
  , orderComment      :: !Text
  , orderState          :: !(Maybe Int)      -- ^ ORDER_STATE (started/placed/filled/...).
  , orderTimeSetup      :: !(Maybe Integer)  -- ^ ORDER_TIME_SETUP (server seconds).
  , orderTimeExpiration :: !(Maybe Integer)  -- ^ ORDER_TIME_EXPIRATION (server seconds, 0 = GTC).
  , orderTypeTime       :: !(Maybe Int)      -- ^ ORDER_TYPE_TIME (GTC/day/specified/...).
  , orderTypeFilling    :: !(Maybe Int)      -- ^ ORDER_TYPE_FILLING (FOK/IOC/RETURN).
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON OrderInfoResponse where
  parseJSON = withObject "OrderInfoResponse" $ \o ->
    OrderInfoResponse
      <$> o .: "ticket"
      <*> o .: "symbol"
      <*> o .: "type"
      -- The MQL5 file-bridge EA reports pending-order volume as
      -- @volume_current@\/@volume_initial@ (no @volume@ key); the Python bridge
      -- uses @volume@.  Accept all three so 'orders_get' parses on both channels.
      <*> (o .: "volume" <|> o .: "volume_current" <|> o .: "volume_initial")
      <*> o .: "price_open"
      -- The file-bridge EA omits @price_current@ for pending orders; default to
      -- the open price rather than failing the whole parse.
      <*> (o .:? "price_current" .!= 0 >>= \pc ->
             if pc /= 0 then pure pc else o .: "price_open")
      <*> o .: "sl"
      <*> o .: "tp"
      <*> o .: "magic"
      <*> o .:? "comment" .!= ""
      <*> o .:? "state"
      <*> o .:? "time_setup"
      <*> o .:? "time_expiration"
      <*> o .:? "type_time"
      <*> o .:? "type_filling"


-- | Response from orders_get action
data OrdersGetResponse = OrdersGetResponse
  { ordersGetSuccess :: !Bool
  , ordersGetCount   :: !Int
  , ordersGetOrders  :: ![OrderInfoResponse]
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON OrdersGetResponse where
  parseJSON = withObject "OrdersGetResponse" $ \o ->
    OrdersGetResponse
      <$> o .: "success"
      <*> o .: "count"
      <*> o .: "orders"
