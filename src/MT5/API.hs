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
  , orderCheck
  ) where

import           Control.DeepSeq
import           Control.Monad     (replicateM)
import           GHC.Generics

import           MT5.Communication
import           MT5.Data


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
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (unpickle' "Int" <$> receive)
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
        <*> (unpickle' "Int" <$> receive)
        <*> (unpickle' "Integer" <$> receive)
        <*> (unpickle' "Integer" <$> receive)
        <*> (unpickle' "Integer" <$> receive)
        <*> (unpickle' "Int" <$> receive)
        <*> (unpickle' "Int" <$> receive)
        <*> (unpickle' "Int" <$> receive)
        <*> (unpickle' "Int" <$> receive)
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

symbolsGet :: String -> IO [SymbolInfo]
symbolsGet group = do
  send "SYMBOLS_GET"
  send group
  len <- unpickle' "Int" <$> receive
  replicateM len readSymbolInfo


symbolInfo :: String -> IO String
symbolInfo symbol = do
  send "SYMBOL_INFO"
  send symbol
  unpickle' "String" <$> receive


symbolSelect :: String -> IO Bool
symbolSelect symbol = do
  send "SYMBOL_SELECT"
  send symbol
  unpickle' "Bool" <$> receive

orderCheck :: MqlTradeRequest -> IO ()
orderCheck MqlTradeRequest{..} = do
  send "ORDER_CHECK"
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


-- TODO: CancelOrderPOST, CancelAllOrdersPOST, CurrentPriceGET, InstrumentCandleGET,


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
--    4003:    def execute(self,command:str):
