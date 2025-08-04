module MT5.Data.AccountInfo
  ( AccountInfo(..)
  , AccountMarginMode(..)
  , AccountStopoutMode(..)
  , AccountTradeMode(..)
  ) where

data AccountMarginMode
  = ACCOUNT_MARGIN_MODE_RETAIL_NETTING
  | ACCOUNT_MARGIN_MODE_EXCHANGE
  | ACCOUNT_MARGIN_MODE_RETAIL_HEDGING
  deriving (Show, Eq, Ord, Enum)

data AccountStopoutMode
  = ACCOUNT_STOPOUT_MODE_PERCENT
  | ACCOUNT_STOPOUT_MODE_MONEY
  deriving (Show, Eq, Ord, Enum)

data AccountTradeMode
  = ACCOUNT_TRADE_MODE_DEMO
  | ACCOUNT_TRADE_MODE_CONTEST
  | ACCOUNT_TRADE_MODE_REAL
  deriving (Show, Eq, Ord, Enum)


data AccountInfo = AccountInfo
  { accInfoLogin              :: Int
  , accInfoTrade_mode         :: AccountTradeMode -- ^ ENUM_ACCOUNT_TRADE_MODE: Demo, Contest, Real
  , accInfoLeverage           :: Int
  , accInfoLimit_orders       :: Int
  , accInfoMargin_so_mode     :: AccountStopoutMode -- ^ ENUM_ACCOUNT_STOPOUT_MODE: Percent, Money
  , accInfoTrade_allowed      :: Bool
  , accInfoTrade_expert       :: Bool
  , accInfoMargin_mode        :: AccountMarginMode -- ^ NUM_ACCOUNT_MARGIN_MODE: RetailNetting, Exchange, RetailHedging
  , accInfoCurrency_digits    :: Int
  , accInfoFifo_close         :: Bool
  , accInfoBalance            :: Double
  , accInfoCredit             :: Double
  , accInfoProfit             :: Double
  , accInfoEquity             :: Double
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
