{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module MT5.API
  ( MT5Login(..)
  , initialize
  , login
  , MT5AccountInfo(..)
  , accountInfo
  ) where

import           Control.DeepSeq
import           GHC.Generics
import           MT5.Communication

data MT5Login =
  MT5Login
    { account  :: String
    , password :: String
    -- , server   :: Maybe String
    -- , timeout  :: Maybe Int
    } deriving (Show, Eq, NFData, Generic)


initialize :: IO (Either String ())
initialize = do
  send "INITIALIZE"
  res <- unpickle' "Bool" <$> receive
  if res
    then return $ Right ()
    else Left <$> getError "failed to initialize to account #{}, error code: {}"


login :: MT5Login -> IO (Either String ())
login MT5Login {..} = do
  send "LOGIN"
  send account
  send password
  res <- unpickle' "Bool" <$> receive
  case res of
    True  -> return $ Right ()
    False -> Left <$> getError "failed to connect to account #{}, error code: {}"

  -- send (server login)
  -- send (timeout login)

data MT5AccountInfo =
  MT5AccountInfo
    { accInfoLogin              :: Int
    , accInfoTrade_mode         :: Int
    , accInfoLeverage           :: Int
    , accInfoLimit_orders       :: Int
    , accInfoMargin_so_mode     :: Int
    , accInfoTrade_allowed      :: Bool
    , accInfoTrade_expert       :: Bool
    , accInfoMargin_mode        :: Int
    , accInfoCurrency_digits    :: Int
    , accInfoFifo_close         :: Bool
    , accInfoBalance            :: Double
    , accInfoCredit             :: Double
    , accInfoProfit             :: Double
    -- , accInfoEquity             :: Double
    , accInfoMargin             :: Double
    , accInfoMargin_free        :: Double
    , accInfoMargin_level       :: Double
    , accInfoMargin_so_call     :: Double
    , accInfoMargin_so_so       :: Double
    , accInfoMargin_initial     :: Double
    , accInfoMargin_maintenance :: Double
    , accInfoAssets             :: Double
    , accInfoLiabilities        :: Double
    , accInfoCommission_blocked :: Double
    , accInfoName               :: String
    , accInfoServer             :: String
    , accInfoCurrency           :: String
    , accInfoCompany            :: String
    } deriving (Show, Eq)

accountInfo :: IO MT5AccountInfo
accountInfo = do
  send "ACCOUNT_INFO"
  MT5AccountInfo <$>
    (unpickle' "Int"    <$> receive) <*>  -- 'login': 173941
    (unpickle' "Int"    <$> receive) <*>  -- 'trade_mode': 2
    (unpickle' "Int" <$> receive) <*>  -- 'leverage': 100
    (unpickle' "Int"    <$> receive) <*>  -- 'limit_orders': 0
    (unpickle' "Int"    <$> receive) <*>  -- 'margin_so_mode': 0
    (unpickle' "Bool"   <$> receive) <*>  -- 'trade_allowed': False
    (unpickle' "Bool"   <$> receive) <*>  -- 'trade_expert': True
    (unpickle' "Int"    <$> receive) <*>  -- 'margin_mode': 2
    (unpickle' "Int"    <$> receive) <*>  -- 'currency_digits': 2
    (unpickle' "Bool"   <$> receive) <*>  -- 'fifo_close': False
    (unpickle' "Double" <$> receive) <*>  -- 'balance': 330.43
    (unpickle' "Double" <$> receive) <*>  -- 'credit': 0.0
    (unpickle' "Double" <$> receive) <*>  -- 'profit': 29.61
    -- (unpickle' "Double" <$> receive) <*>  -- 'equity': 360.04
    (unpickle' "Double" <$> receive) <*>  -- 'margin': 198.45
    (unpickle' "Double" <$> receive) <*>  -- 'margin_free': 161.59
    (unpickle' "Double" <$> receive) <*>  -- 'margin_level': 181.4260519022424
    (unpickle' "Double" <$> receive) <*>  -- 'margin_so_call': 100.0
    (unpickle' "Double" <$> receive) <*>  -- 'margin_so_so': 50.0
    (unpickle' "Double" <$> receive) <*>  -- 'margin_initial': 0.0
    (unpickle' "Double" <$> receive) <*>  -- 'margin_maintenance': 0.0
    (unpickle' "Double" <$> receive) <*>  -- 'assets': 0.0
    (unpickle' "Double" <$> receive) <*>  -- 'liabilities': 0.0
    (unpickle' "Double" <$> receive) <*>  -- 'commission_blocked': 0.0
    (unpickle' "String" <$> receive) <*>  -- 'name': 'EU/M173941/EUR'
    (unpickle' "String" <$> receive) <*>  -- 'server': 'OANDATMS-MT5'
    (unpickle' "String" <$> receive) <*>  -- 'currency': 'EUR'
    (unpickle' "String" <$> receive)      -- 'company': 'OANDA TMS Brokers S.A.'

getError :: String -> IO String
getError formatString = do
  send "ERROR"
  send formatString
  unpickle' "String" <$> receive

