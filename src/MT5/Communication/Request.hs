{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Type-safe request types for MT5 file-based communication
--
-- This module provides domain-specific request types with smart constructors
-- for validation, ensuring type safety and preventing invalid requests from
-- being sent to the MT5 EA.
module MT5.Communication.Request
    ( -- * Order Management Requests
      OrderSendRequest(..)
    , mkOrderSendRequest
      -- * Position Management Requests
    , PositionCloseRequest(..)
    , mkPositionCloseRequest
    , PositionClosePartialRequest(..)
    , mkPositionClosePartialRequest
    , PositionModifyRequest(..)
    , mkPositionModifyRequest
      -- * Symbol Information Requests
    , SymbolInfoRequest(..)
    , mkSymbolInfoRequest
    , SymbolSelectRequest(..)
    , mkSymbolSelectRequest
    , SymbolsGetRequest(..)
    , mkSymbolsGetRequest
      -- * Historical Data Requests
    , CandlesGetRequest(..)
    , CandleMode(..)
    , mkCandlesGetRequest
      -- * Account & Position Queries
    , AccountInfoRequest(..)
    , mkAccountInfoRequest
    , PositionsGetRequest(..)
    , mkPositionsGetRequest
    , OrdersGetRequest(..)
    , mkOrdersGetRequest
      -- * Validation Errors
    , RequestError(..)
    ) where

import           Control.DeepSeq (NFData)
import           Data.Aeson      (ToJSON(..), Value(..), object, (.=))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)

import           MT5.Data.OrderType
import           MT5.Data.OrderTypeFilling
import           MT5.Data.OrderTypeTime


-- | Request validation errors
data RequestError
  = InvalidVolume Double       -- ^ Volume must be positive
  | InvalidPrice Double        -- ^ Price must be non-negative
  | InvalidSymbol Text         -- ^ Symbol name is empty or invalid
  | InvalidTicket Integer      -- ^ Ticket must be positive
  | InvalidCount Integer       -- ^ Count must be positive
  | InvalidTimeRange UTCTime UTCTime  -- ^ Start time must be before end time
  deriving (Show, Eq, Generic, NFData)


--------------------------------------------------------------------------------
-- Order Management Requests
--------------------------------------------------------------------------------

-- | Request to send a trading order
data OrderSendRequest = OrderSendRequest
  { orderSendSymbol      :: !Text
  , orderSendVolume      :: !Double
  , orderSendOrderType   :: !OrderType
  , orderSendPrice       :: !Double
  , orderSendSl          :: !Double
  , orderSendTp          :: !Double
  , orderSendDeviation   :: !Int
  , orderSendTypeFilling :: !OrderTypeFilling
  , orderSendTypeTime    :: !OrderTypeTime
  , orderSendComment     :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON OrderSendRequest where
  toJSON req = object
    [ "symbol"       .= orderSendSymbol req
    , "volume"       .= orderSendVolume req
    , "type"         .= fromEnum (orderSendOrderType req)
    , "price"        .= orderSendPrice req
    , "sl"           .= orderSendSl req
    , "tp"           .= orderSendTp req
    , "deviation"    .= orderSendDeviation req
    , "type_filling" .= fromEnum (orderSendTypeFilling req)
    , "type_time"    .= fromEnum (orderSendTypeTime req)
    , "comment"      .= orderSendComment req
    ]

-- | Smart constructor for OrderSendRequest with validation
mkOrderSendRequest :: Text -> Double -> OrderType -> Double -> Double -> Double 
                   -> Int -> OrderTypeFilling -> OrderTypeTime -> Text 
                   -> Either RequestError OrderSendRequest
mkOrderSendRequest symbol volume orderType price sl tp deviation filling typeTime comment
  | T.null symbol          = Left (InvalidSymbol symbol)
  | volume <= 0            = Left (InvalidVolume volume)
  | price < 0              = Left (InvalidPrice price)
  | sl < 0                 = Left (InvalidPrice sl)
  | tp < 0                 = Left (InvalidPrice tp)
  | otherwise              = Right $ OrderSendRequest symbol volume orderType price sl tp deviation filling typeTime comment


--------------------------------------------------------------------------------
-- Position Management Requests
--------------------------------------------------------------------------------

-- | Request to close a position completely
newtype PositionCloseRequest = PositionCloseRequest
  { positionCloseTicket :: Integer
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON PositionCloseRequest where
  toJSON req = object
    [ "ticket" .= positionCloseTicket req ]

-- | Smart constructor for PositionCloseRequest
mkPositionCloseRequest :: Integer -> Either RequestError PositionCloseRequest
mkPositionCloseRequest ticket
  | ticket <= 0 = Left (InvalidTicket ticket)
  | otherwise   = Right (PositionCloseRequest ticket)


-- | Request to partially close a position
data PositionClosePartialRequest = PositionClosePartialRequest
  { positionClosePartialTicket :: !Integer
  , positionClosePartialVolume :: !Double
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON PositionClosePartialRequest where
  toJSON req = object
    [ "ticket" .= positionClosePartialTicket req
    , "volume" .= positionClosePartialVolume req
    ]

-- | Smart constructor for PositionClosePartialRequest
mkPositionClosePartialRequest :: Integer -> Double -> Either RequestError PositionClosePartialRequest
mkPositionClosePartialRequest ticket volume
  | ticket <= 0 = Left (InvalidTicket ticket)
  | volume <= 0 = Left (InvalidVolume volume)
  | otherwise   = Right (PositionClosePartialRequest ticket volume)


-- | Request to modify position SL/TP
data PositionModifyRequest = PositionModifyRequest
  { positionModifyTicket :: !Integer
  , positionModifySl     :: !Double
  , positionModifyTp     :: !Double
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON PositionModifyRequest where
  toJSON req = object
    [ "ticket" .= positionModifyTicket req
    , "sl"     .= positionModifySl req
    , "tp"     .= positionModifyTp req
    ]

-- | Smart constructor for PositionModifyRequest
--
-- Note: SL and TP values of 0.0 are allowed (means remove SL/TP)
mkPositionModifyRequest :: Integer -> Double -> Double -> Either RequestError PositionModifyRequest
mkPositionModifyRequest ticket sl tp
  | ticket <= 0 = Left (InvalidTicket ticket)
  | sl < 0      = Left (InvalidPrice sl)
  | tp < 0      = Left (InvalidPrice tp)
  | otherwise   = Right (PositionModifyRequest ticket sl tp)


--------------------------------------------------------------------------------
-- Symbol Information Requests
--------------------------------------------------------------------------------

-- | Request information about a specific symbol
newtype SymbolInfoRequest = SymbolInfoRequest
  { symbolInfoSymbol :: Text
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON SymbolInfoRequest where
  toJSON req = object
    [ "symbol" .= symbolInfoSymbol req ]

-- | Smart constructor for SymbolInfoRequest
mkSymbolInfoRequest :: Text -> Either RequestError SymbolInfoRequest
mkSymbolInfoRequest symbol
  | T.null symbol = Left (InvalidSymbol symbol)
  | otherwise     = Right (SymbolInfoRequest symbol)


-- | Request to enable/disable symbol in Market Watch
data SymbolSelectRequest = SymbolSelectRequest
  { symbolSelectSymbol :: !Text
  , symbolSelectEnable :: !Bool
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON SymbolSelectRequest where
  toJSON req = object
    [ "symbol" .= symbolSelectSymbol req
    , "enable" .= symbolSelectEnable req
    ]

-- | Smart constructor for SymbolSelectRequest
mkSymbolSelectRequest :: Text -> Bool -> Either RequestError SymbolSelectRequest
mkSymbolSelectRequest symbol enable
  | T.null symbol = Left (InvalidSymbol symbol)
  | otherwise     = Right (SymbolSelectRequest symbol enable)


-- | Request to get list of available symbols
data SymbolsGetRequest = SymbolsGetRequest
  { symbolsGetGroup :: !(Maybe Text)  -- ^ Optional group filter
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON SymbolsGetRequest where
  toJSON req = object
    [ "group" .= symbolsGetGroup req ]

-- | Smart constructor for SymbolsGetRequest
mkSymbolsGetRequest :: Maybe Text -> SymbolsGetRequest
mkSymbolsGetRequest = SymbolsGetRequest


--------------------------------------------------------------------------------
-- Historical Data Requests
--------------------------------------------------------------------------------

-- | Mode for fetching candles
data CandleMode
  = CandleModeRange    -- ^ Fetch candles within time range
  | CandleModeFrom     -- ^ Fetch candles from start time
  | CandleModeFromPos  -- ^ Fetch candles from position
  deriving (Show, Eq, Ord, Enum, Generic, NFData)

-- | Request to fetch historical candle data
data CandlesGetRequest = CandlesGetRequest
  { candlesGetSymbol    :: !Text
  , candlesGetTimeframe :: !Text       -- ^ e.g., "M1", "H1", "D1"
  , candlesGetMode      :: !CandleMode
  , candlesGetStart     :: !(Maybe UTCTime)  -- ^ For range and from modes
  , candlesGetEnd       :: !(Maybe UTCTime)  -- ^ For range mode only
  , candlesGetPos       :: !(Maybe Int)      -- ^ For from_pos mode
  , candlesGetCount     :: !Int             -- ^ Number of candles to fetch
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON CandlesGetRequest where
  toJSON req = object
    [ "symbol"    .= candlesGetSymbol req
    , "timeframe" .= candlesGetTimeframe req
    , "mode"      .= modeStr (candlesGetMode req)
    , "start"     .= candlesGetStart req
    , "end"       .= candlesGetEnd req
    , "pos"       .= candlesGetPos req
    , "count"     .= candlesGetCount req
    ]
    where
      modeStr CandleModeRange   = "range" :: Text
      modeStr CandleModeFrom    = "from"
      modeStr CandleModeFromPos = "from_pos"

-- | Smart constructor for CandlesGetRequest
mkCandlesGetRequest :: Text -> Text -> CandleMode -> Maybe UTCTime -> Maybe UTCTime 
                    -> Maybe Int -> Int -> Either RequestError CandlesGetRequest
mkCandlesGetRequest symbol timeframe mode start end pos count
  | T.null symbol       = Left (InvalidSymbol symbol)
  | T.null timeframe    = Left (InvalidSymbol timeframe)  -- Reuse for simplicity
  | count <= 0          = Left (InvalidCount (fromIntegral count))
  | mode == CandleModeRange && invalidRange = Left (InvalidTimeRange (maybe undefined id start) (maybe undefined id end))
  | otherwise           = Right $ CandlesGetRequest symbol timeframe mode start end pos count
  where
    invalidRange = case (start, end) of
      (Just s, Just e) -> s >= e
      _                -> False


--------------------------------------------------------------------------------
-- Account & Position Query Requests
--------------------------------------------------------------------------------

-- | Request to get account information
--
-- This request has no parameters - it retrieves current account info
data AccountInfoRequest = AccountInfoRequest
  deriving (Show, Eq, Generic, NFData)

instance ToJSON AccountInfoRequest where
  toJSON AccountInfoRequest = object []

-- | Smart constructor for AccountInfoRequest
mkAccountInfoRequest :: AccountInfoRequest
mkAccountInfoRequest = AccountInfoRequest


-- | Request to get open positions
data PositionsGetRequest = PositionsGetRequest
  { positionsGetSymbol :: !(Maybe Text)  -- ^ Optional symbol filter
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON PositionsGetRequest where
  toJSON req = object
    [ "symbol" .= positionsGetSymbol req ]

-- | Smart constructor for PositionsGetRequest
mkPositionsGetRequest :: Maybe Text -> PositionsGetRequest
mkPositionsGetRequest = PositionsGetRequest


-- | Request to get pending orders
data OrdersGetRequest = OrdersGetRequest
  { ordersGetSymbol :: !(Maybe Text)  -- ^ Optional symbol filter
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON OrdersGetRequest where
  toJSON req = object
    [ "symbol" .= ordersGetSymbol req ]

-- | Smart constructor for OrdersGetRequest
mkOrdersGetRequest :: Maybe Text -> OrdersGetRequest
mkOrdersGetRequest = OrdersGetRequest
