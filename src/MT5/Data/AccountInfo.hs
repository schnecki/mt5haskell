module MT5.Data.AccountInfo
  ( AccountInfo(..)
  ) where


data AccountInfo = AccountInfo
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
