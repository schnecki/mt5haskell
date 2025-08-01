{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module MT5.API
  ( MT5Login(..)
  , initialize
  , login
  , accountInfo
  , positionsGet
  , symbolInfo
  , symbolsGet
  , symbolSelect
  , cancelOrderPOST
  , cancelAllOrdersPOST
  , orderCheck
  , orderSend
  , ordersGet
  , currentPriceGET
  , getCandleDataRange
  , getCandleDataFrom
  , getCandleDataRecent
  ) where

import           Control.DeepSeq
import           Control.Monad     (replicateM)
import           Data.List         (isPrefixOf)
import qualified Data.Text         as T
import           Data.Time         (UTCTime)
import           Data.Time.Format  (formatTime, defaultTimeLocale)
import GHC.Generics ( Generic )


import MT5.Communication ( send, receive, unpickle' ) 
import MT5.Data
    ( CurrentPrice(..),
      TradePosition(TradePosition),
      AccountInfo(AccountInfo),
      MqlTradeRequest(..),
      TradeOrder(TradeOrder, tradeOrderTicket),
      OrderSendResult,
      readOrderSendResult,
      SymbolInfo,
      readSymbolInfo )
import MT5.Data.Candle
    ( MT5CandleData(MT5CandleData), MT5Candle(MT5Candle) )
import MT5.Data.Granularity ( MT5Granularity, toMT5TimeframeInt )
import MT5.Util ( secondsToUTCTime, mscToUTCTime )

type Symbol = String
type Ticket = Int


data MT5Login = MT5Login
  { account  :: String
  , password :: String
    -- , server   :: Maybe String
    -- , timeout  :: Maybe Int
  } deriving (Show, Eq, NFData, Generic)

-- | def initialize(self,*args,**kwargs):
initialize :: IO (Either String ())
initialize = do
  send "INITIALIZE"
  res <- unpickle' "Bool" <$> receive
  if res
    then return $ Right ()
    else Left <$> getError "failed to initialize to account #{}, error code: {}"

-- | Function login
login :: MT5Login -> IO (Either String ())
login MT5Login {..} = do
  send "LOGIN"
  send account
  send password
  res <- unpickle' "Bool" <$> receive
  if res
    then return $ Right ()
    else Left <$> getError "failed to connect to account #{}, error code: {}"


accountInfo :: IO AccountInfo
accountInfo = do
  send "ACCOUNT_INFO"
  AccountInfo
    <$> (unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "Double" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)

getError :: String -> IO String
getError formatString = do
  send "ERROR"
  send formatString
  unpickle' "String" <$> receive


positionsGet :: IO [TradePosition]
positionsGet = do
  send "POSITIONS_GET"
  len <- unpickle' "Int" <$> receive
  replicateM len
    $ TradePosition
        <$> (unpickle' "Int" <$> receive)
        <*> (secondsToUTCTime . unpickle' "Integer" <$> receive)
        <*> (mscToUTCTime . unpickle' "Integer" <$> receive)
        <*> (secondsToUTCTime . unpickle' "Integer" <$> receive)
        <*> (mscToUTCTime . unpickle' "Integer" <$> receive)
        <*> (toEnum . unpickle' "Int" <$> receive)
        <*> (unpickle' "Int" <$> receive)
        <*> (unpickle' "Int" <$> receive)
        <*> (toEnum . unpickle' "Int" <$> receive)
        <*> (unpickle' "Double" <$> receive)
        <*> (unpickle' "Double" <$> receive)
        <*> (unpickle' "Double" <$> receive)
        <*> (unpickle' "Double" <$> receive)
        <*> (unpickle' "Double" <$> receive)
        <*> (unpickle' "Double" <$> receive)
        <*> (unpickle' "Double" <$> receive)
        <*> (unpickle' "String" <$> receive)
        <*> (unpickle' "String" <$> receive)
        <*> (unpickle' "String" <$> receive)

-- | Get active orders with the ability to filter by symbol or ticket. You can specify the symbol or the ticket if you
-- desire.
ordersGet :: Maybe Symbol -> Maybe Ticket -> IO [TradeOrder]
ordersGet mInstr mTicket = do
  case (mInstr, mTicket) of
    (Just instr, Nothing) -> do
      send "ORDERS_GET_SYMBOL"
      send instr
    (_, Just ticket) -> do
      send "ORDERS_GET_TICKET"
      send (show ticket)
    _ -> do
      send "ORDERS_GET"
  len <- unpickle' "Int" <$> receive
  replicateM len
        $ TradeOrder
            <$> (unpickle' "Int" <$> receive)
            <*> (secondsToUTCTime . unpickle' "Integer" <$> receive)
            <*> (mscToUTCTime . unpickle' "Integer" <$> receive)
            <*> (unpickle' "Int" <$> receive)
            <*> (toEnum . unpickle' "Int" <$> receive)
            <*> (unpickle' "Integer" <$> receive)
            <*> (unpickle' "Int" <$> receive)
            <*> (toEnum . unpickle' "Int" <$> receive)
            <*> (unpickle' "Int" <$> receive)
            <*> (unpickle' "Double" <$> receive)
            <*> (unpickle' "Double" <$> receive)
            <*> (unpickle' "Double" <$> receive)
            <*> (unpickle' "Double" <$> receive)
            <*> (unpickle' "Double" <$> receive)
            <*> (unpickle' "String" <$> receive)
            <*> (unpickle' "String" <$> receive)
            <*> (unpickle' "String" <$> receive)


symbolsGet :: String -> IO [SymbolInfo]
symbolsGet group = do
  send "SYMBOLS_GET"
  send group
  len <- unpickle' "Int" <$> receive
  replicateM len readSymbolInfo


symbolInfo :: String -> IO SymbolInfo
symbolInfo symbol = do
  send "SYMBOL_INFO"
  send symbol
  readSymbolInfo

symbolSelect :: String -> IO Bool
symbolSelect symbol = do
  send "SYMBOL_SELECT"
  send symbol
  unpickle' "Bool" <$> receive

-- | Get current price information for a trading symbol
--
-- This function retrieves real-time price data including bid, ask, spread,
-- volume and timestamp information for the specified symbol.
--
-- ==== __Examples__
--
-- >>> currentPriceGET "EURUSD"
-- Right CurrentPrice{cpSymbol="EURUSD", cpBid=1.0850, cpAsk=1.0852, cpSpread=0.0002, ...}
--
-- >>> currentPriceGET "INVALID_SYMBOL"  
-- Left "No tick data available for INVALID_SYMBOL"
currentPriceGET :: String -> IO (Either String CurrentPrice)
currentPriceGET symbol = do
  -- Follow established command pattern (uppercase commands)
  send "SYMBOL_INFO_TICK"
  send symbol
  
  -- Read the response following the established pattern
  result <- unpickle' "String" <$> receive
  
  -- Check if response indicates an error
  if "error:" `isPrefixOf` result
    then return $ Left (drop 6 result) -- Remove "error:" prefix
    else parseCurrentPriceFromFields symbol

-- | Parse current price by reading individual fields from Python server
-- Following the established pattern of reading fields sequentially
parseCurrentPriceFromFields :: String -> IO (Either String CurrentPrice)
parseCurrentPriceFromFields symbol = do
  bid        <- unpickle' "Double" <$> receive  -- bid price
  ask        <- unpickle' "Double" <$> receive  -- ask price
  lastPrice  <- unpickle' "Double" <$> receive  -- last price
  volume     <- unpickle' "Int" <$> receive     -- volume
  timeEpoch  <- unpickle' "Integer" <$> receive -- time (seconds)
  timeMsc    <- unpickle' "Integer" <$> receive -- time_msc (milliseconds)
  flags      <- unpickle' "Int" <$> receive     -- flags
  volReal    <- unpickle' "Double" <$> receive  -- volume_real
  
  let utcTime = secondsToUTCTime timeEpoch      -- Convert using existing utility
  let spread = ask - bid                        -- Calculate spread
  
  return $ Right $ CurrentPrice
    { cpSymbol     = T.pack symbol
    , cpBid        = bid
    , cpAsk        = ask
    , cpSpread     = spread
    , cpLast       = lastPrice
    , cpVolume     = volume
    , cpTime       = utcTime  
    , cpTimeMsc    = timeMsc
    , cpFlags      = flags
    , cpVolumeReal = volReal
    }

orderCheck :: MqlTradeRequest -> IO OrderSendResult
orderCheck request = do
  send "ORDER_CHECK"
  sendMqlTradeRequest request
  readOrderSendResult

sendMqlTradeRequest :: MqlTradeRequest -> IO ()
sendMqlTradeRequest MqlTradeRequest {..} = do
  send $ show . fromEnum $ trReqAction
  send $ show trReqMagic
  send $ show trReqOrder
  send   trReqSymbol
  send $ show trReqVolume
  send $ show trReqPrice
  send $ show trReqStoplimit
  send $ show trReqSl
  send $ show trReqTp
  send $ show trReqDeviation
  send $ show . fromEnum $ trReqType
  send $ show . fromEnum $ trReqTypeFilling
  send $ show . fromEnum $ trReqTypeTime
  send $ show trReqExpiration
  send   trReqComment
  send $ show trReqPosition
  send $ show trReqPositionBy

orderSend :: MqlTradeRequest -> IO OrderSendResult
orderSend request = do
  send "ORDER_SEND"
  sendMqlTradeRequest request
  readOrderSendResult

-- | Cancel a pending order by ticket number
-- 
-- Sends a cancellation request for the specified order ticket.
-- Only works for pending orders that haven't been executed yet.
-- 
-- Returns 'TRADE_RETCODE_DONE' on successful cancellation.
cancelOrderPOST :: Int                -- ^ Order ticket number to cancel
                -> IO OrderSendResult -- ^ Result of the cancellation request
cancelOrderPOST orderTicket = do
  send "ORDER_CANCEL"
  send (show orderTicket)
  readOrderSendResult

-- | Cancel all pending orders in the account
--
-- Retrieves all pending orders and attempts to cancel each one individually.
-- Returns a list of cancellation results, one per order.
--
-- * Empty list if no pending orders exist
-- * Partial results if some cancellations fail
-- * Each result contains detailed information about the cancellation attempt
--
-- @since 0.1.0.0
cancelAllOrdersPOST :: IO [OrderSendResult]
cancelAllOrdersPOST = do
  -- Phase 1: Get all pending orders
  orders <- ordersGet Nothing Nothing
  
  -- Phase 2: Cancel each order individually
  mapM cancelSingleOrder orders
  where
    cancelSingleOrder :: TradeOrder -> IO OrderSendResult
    cancelSingleOrder order = cancelOrderPOST (tradeOrderTicket order)


-- TODO: CurrentPriceGET, InstrumentCandleGET,


-- FIX MAPPING: OpenTradesGET, AccountSummaryGET


-- 38 matches for "^[ ]*def" in buffer: __init__.py
--     357:    def __init__(self,host='localhost',port=18812):
--     360:    default = localhost
--     362:    default = 18812
--     369:    def __del__(self):
--     372:    def initialize(self,*args,**kwargs):
--     468:    def login(self,*args,**kwargs):
--     595:    def shutdown(self,*args,**kwargs):
--     640:    def version(self,*args,**kwargs):
--     735:    def last_error(self,*args,**kwargs):
--     795:    def account_info(self,*args,**kwargs):
--     929:    def terminal_info(self,*args,**kwargs):
--    1049:    def symbols_total(self,*args,**kwargs):
--    1100:    def symbols_get(self,*args,**kwargs):
--    1218:    def symbol_info(self,*args,**kwargs):
--    1393:    def symbol_info_tick(self,*args,**kwargs):
--    1474:    def symbol_select(self,*args,**kwargs):
--    1673:    def market_book_add(self,*args,**kwargs):
--    1706:    def market_book_get(self,*args,**kwargs):
--    1846:    def market_book_release(self,symbol):
--    1879:    def copy_rates_from(self,symbol, timeframe, date_from, count):
--    2037:    def copy_rates_from_pos(self,symbol,timeframe,start_pos,count):
--    2158:    def copy_rates_range(self,symbol, timeframe, date_from, date_to):
--    2294:    def copy_ticks_from(self,symbol, date_from, count, flags):
--    2449:    def copy_ticks_range(self,symbol, date_from, date_to, flags):
--    2584:    def orders_total(self,*args,**kwargs):
--    2635:    def orders_get(self,*args,**kwargs):
--    2765:    def order_calc_margin(self,*args,**kwargs):
--    2887:    def order_calc_profit(self,*args,**kwargs):
--    3011:    def order_check(self,*args,**kwargs):
--    3176:    def order_send(self,request):
--    3385:    def positions_total(self,*args,**kwargs):
--    3436:    def positions_get(self,*args,**kwargs):
--    3562:    def history_orders_total(self,date_from, date_to):
--    3627:    def history_orders_get(self,*args,**kwargs):
--    3767:    def history_deals_total(self,date_from, date_to):
--    3834:    def history_deals_get(self,*args,**kwargs):
--    4000:    def eval(self,command:str):
--    4003:    def execute(self,command:str)

-- | Get candlestick data using time range
--
-- Retrieves OHLC candlestick data for a specific time range using
-- MT5's copy_rates_range function. Following the same communication
-- pattern as currentPriceGET for consistency and reliability.
--
-- This function sends a COPY_RATES_RANGE command to the Python server
-- along with the symbol, timeframe, and date range parameters.
--
-- ==== __Examples__
--
-- >>> import Data.Time
-- >>> from <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2023-01-01 00:00:00"
-- >>> to <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2023-01-02 00:00:00"  
-- >>> getCandleDataRange "EURUSD" M5 from to
-- Right (MT5CandleData {mt5Candles = [...], mt5Symbol = "EURUSD"})
--
-- >>> getCandleDataRange "INVALID" M5 from to
-- Left "No rate data available for INVALID, MT5 error: ..."
getCandleDataRange :: String          -- ^ Trading symbol (e.g., "EURUSD")
                   -> MT5Granularity  -- ^ Timeframe for candles
                   -> UTCTime         -- ^ Start time (inclusive)
                   -> UTCTime         -- ^ End time (inclusive)
                   -> IO (Either String MT5CandleData)
getCandleDataRange symbol granularity fromTime toTime = do
  send "COPY_RATES_RANGE"
  send symbol
  send $ show $ toMT5TimeframeInt granularity
  send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fromTime
  send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" toTime
  
  -- Read response following established pattern (like currentPriceGET)
  result <- unpickle' "String" <$> receive
  
  if "error:" `isPrefixOf` result
    then return $ Left (drop 6 result)
    else parseCandleDataFromFields symbol

-- | Get candlestick data using count from specific time
--
-- Retrieves a specific number of candlesticks starting from a given time
-- using MT5's copy_rates_from function. Useful when you need a fixed
-- number of candles from a specific starting point.
--
-- ==== __Examples__
--
-- >>> import Data.Time
-- >>> from <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2023-01-01 00:00:00"
-- >>> getCandleDataFrom "EURUSD" M5 from 100
-- Right (MT5CandleData {mt5Candles = [...], mt5Symbol = "EURUSD"})
getCandleDataFrom :: String          -- ^ Trading symbol
                  -> MT5Granularity  -- ^ Timeframe for candles  
                  -> UTCTime         -- ^ Start time
                  -> Int             -- ^ Number of candles to retrieve (max 5000)
                  -> IO (Either String MT5CandleData)
getCandleDataFrom symbol granularity fromTime count = do
  send "COPY_RATES_FROM"
  send symbol
  send $ show $ toMT5TimeframeInt granularity
  send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fromTime
  send $ show count
  
  result <- unpickle' "String" <$> receive
  
  if "error:" `isPrefixOf` result
    then return $ Left (drop 6 result)
    else parseCandleDataFromFields symbol

-- | Get recent candlestick data (most recent count candles)
--
-- Retrieves the most recent candlesticks using MT5's copy_rates_from_pos
-- function. This is the most efficient way to get recent market data
-- without specifying exact timestamps.
--
-- ==== __Examples__
--
-- >>> getCandleDataRecent "EURUSD" M5 50  -- Last 50 M5 candles
-- Right (MT5CandleData {mt5Candles = [...], mt5Symbol = "EURUSD"})
--
-- >>> getCandleDataRecent "GBPUSD" H1 24  -- Last 24 H1 candles (24 hours)
-- Right (MT5CandleData {mt5Candles = [...], mt5Symbol = "GBPUSD"})
getCandleDataRecent :: String          -- ^ Trading symbol
                    -> MT5Granularity  -- ^ Timeframe for candles
                    -> Int             -- ^ Number of recent candles (max 5000)
                    -> IO (Either String MT5CandleData)
getCandleDataRecent symbol granularity count = do
  send "COPY_RATES_FROM_POS"
  send symbol
  send $ show $ toMT5TimeframeInt granularity
  send "0"  -- start from most recent (position 0)
  send $ show count
  
  result <- unpickle' "String" <$> receive
  
  if "error:" `isPrefixOf` result
    then return $ Left (drop 6 result)
    else parseCandleDataFromFields symbol

-- | Parse candle data by reading individual fields from Python server
--
-- Following the established pattern of reading fields sequentially
-- (like parseCurrentPriceFromFields). This maintains consistency with
-- the existing communication protocol and ensures reliable data transfer.
--
-- The function reads the candle count first, then iterates to read
-- each candle's OHLC data sequentially, using the existing utility
-- functions for time conversion.
parseCandleDataFromFields :: String -> IO (Either String MT5CandleData)
parseCandleDataFromFields symbol = do
  candleCount <- unpickle' "Int" <$> receive
  candles <- replicateM candleCount readSingleCandle
  return $ Right $ MT5CandleData candles symbol
  where
    readSingleCandle :: IO MT5Candle
    readSingleCandle = MT5Candle
      <$> (secondsToUTCTime . unpickle' "Integer" <$> receive)  -- time (using existing utility)
      <*> (unpickle' "Double" <$> receive)                     -- open
      <*> (unpickle' "Double" <$> receive)                     -- high  
      <*> (unpickle' "Double" <$> receive)                     -- low
      <*> (unpickle' "Double" <$> receive)                     -- close
      <*> (unpickle' "Int" <$> receive)                        -- volume
