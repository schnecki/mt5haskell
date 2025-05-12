module MT5.Data.SymbolInfo
    ( SymbolInfo (..)
    , SymbolChartMode (..)
    , SymbolCalcMode(..)
    , toSymbolCalcMode
    , readSymbolInfo
    , SymbolOptionMode (..)
    , SymbolTradeMode(..)
    , SymbolOptionRight (..)
    , SymbolOrders(..)
    , SymbolSwapMode(..)
    , SymbolTradeExecutionMode(..)
    ) where

import           Data.Time.Clock   (UTCTime)

import           MT5.Communication
import           MT5.Util

data SymbolChartMode
  = SYMBOL_CHART_MODE_BID
  | SYMBOL_CHART_MODE_LAST
  deriving (Show, Eq, Ord, Enum)

data SymbolCalcMode =
    SYMBOL_CALC_MODE_FOREX
    | SYMBOL_CALC_MODE_FUTURES
    | SYMBOL_CALC_MODE_CFD
    | SYMBOL_CALC_MODE_CFDINDEX
    | SYMBOL_CALC_MODE_CFDLEVERAGE
    | SYMBOL_CALC_MODE_FOREX_NO_LEVERAGE
    | SYMBOL_CALC_MODE_EXCH_STOCKS
    | SYMBOL_CALC_MODE_EXCH_FUTURES
    | SYMBOL_CALC_MODE_EXCH_OPTIONS
    | SYMBOL_CALC_MODE_EXCH_OPTIONS_MARGIN
    | SYMBOL_CALC_MODE_EXCH_BONDS
    | SYMBOL_CALC_MODE_EXCH_STOCKS_MOEX
    | SYMBOL_CALC_MODE_EXCH_BONDS_MOEX
    | SYMBOL_CALC_MODE_SERV_COLLATERAL
  deriving (Show, Eq, Ord, Enum)

toSymbolCalcMode :: Int -> SymbolCalcMode
toSymbolCalcMode x =
  case x of
    0  -> SYMBOL_CALC_MODE_FOREX
    1  -> SYMBOL_CALC_MODE_FUTURES
    2  -> SYMBOL_CALC_MODE_CFD
    3  -> SYMBOL_CALC_MODE_CFDINDEX
    4  -> SYMBOL_CALC_MODE_CFDLEVERAGE
    5  -> SYMBOL_CALC_MODE_FOREX_NO_LEVERAGE
    32 -> SYMBOL_CALC_MODE_EXCH_STOCKS
    33 -> SYMBOL_CALC_MODE_EXCH_FUTURES
    34 -> SYMBOL_CALC_MODE_EXCH_OPTIONS
    36 -> SYMBOL_CALC_MODE_EXCH_OPTIONS_MARGIN
    37 -> SYMBOL_CALC_MODE_EXCH_BONDS
    38 -> SYMBOL_CALC_MODE_EXCH_STOCKS_MOEX
    39 -> SYMBOL_CALC_MODE_EXCH_BONDS_MOEX
    64 -> SYMBOL_CALC_MODE_SERV_COLLATERAL
    _  -> error $ "Unknown symbol calc mode " ++ show x

data SymbolTradeMode
  = SYMBOL_TRADE_MODE_DISABLED
  | SYMBOL_TRADE_MODE_LONGONLY
  | SYMBOL_TRADE_MODE_SHORTONLY
  | SYMBOL_TRADE_MODE_CLOSEONLY
  | SYMBOL_TRADE_MODE_FULL
  deriving (Show, Eq, Ord, Enum)

data SymbolOptionMode
  = SYMBOL_OPTION_MODE_EUROPEAN
  | SYMBOL_OPTION_MODE_AMERICAN
  deriving (Show, Eq, Ord, Enum)


data SymbolOptionRight
  = SYMBOL_OPTION_RIGHT_CALL
  | SYMBOL_OPTION_RIGHT_PUT
  deriving (Show, Eq, Ord, Enum)

data SymbolOrders
  = SYMBOL_ORDERS_GTC
  | SYMBOL_ORDERS_DAILY
  | SYMBOL_ORDERS_DAILY_NO_STOPS
  deriving (Show, Eq, Ord, Enum)


data SymbolSwapMode
  = SYMBOL_SWAP_MODE_DISABLED
  | SYMBOL_SWAP_MODE_POINTS
  | SYMBOL_SWAP_MODE_CURRENCY_SYMBOL
  | SYMBOL_SWAP_MODE_CURRENCY_MARGIN
  | SYMBOL_SWAP_MODE_CURRENCY_DEPOSIT
  | SYMBOL_SWAP_MODE_INTEREST_CURRENT
  | SYMBOL_SWAP_MODE_INTEREST_OPEN
  | SYMBOL_SWAP_MODE_REOPEN_CURRENT
  | SYMBOL_SWAP_MODE_REOPEN_BID
  deriving (Show, Eq, Ord, Enum)

data SymbolTradeExecutionMode
  = SYMBOL_TRADE_EXECUTION_REQUEST
  | SYMBOL_TRADE_EXECUTION_INSTANT
  | SYMBOL_TRADE_EXECUTION_MARKET
  | SYMBOL_TRADE_EXECUTION_EXCHANGE
  deriving (Show, Eq, Ord, Enum)


data SymbolInfo = SymbolInfo
  { symInfoCustom                  :: Bool   -- False
  , symInfoChartMode               :: SymbolChartMode    -- 1
  , symInfoSelect                  :: Bool   -- True
  , symInfoVisible                 :: Bool   -- False
  , symInfoSessionDeals            :: Int    -- 0
  , symInfoSessionBuyOrders        :: SymbolOrders    -- 0
  , symInfoSessionSellOrders       :: SymbolOrders    -- 0
  , symInfoVolume                  :: Int    -- 0
  , symInfoVolumehigh              :: Int    -- 0
  , symInfoVolumelow               :: Int    -- 0
  , symInfoTime                    :: Int    -- 1744412338
  , symInfoDigits                  :: Int    -- 2
  , symInfoSpread                  :: Int    -- 400
  , symInfoSpreadFloat             :: Bool   -- False
  , symInfoTicksBookdepth          :: Int    -- 0
  , symInfoTradeCalcMode           :: SymbolCalcMode    -- 0
  , symInfoTradeMode               :: SymbolTradeMode    -- 0
  , symInfoStartTime               :: UTCTime    -- 0
  , symInfoExpirationTime          :: UTCTime    -- 0
  , symInfoTradeStopsLevel         :: Int    -- 0
  , symInfoTradeFreezeLevel        :: Int    -- 0
  , symInfoTradeExemode            :: SymbolTradeExecutionMode    -- 2
  , symInfoSwapMode                :: SymbolSwapMode    -- 0
  , symInfoSwapRollover3days       :: Int    -- 3
  , symInfoMarginHedgedUseLeg      :: Bool   -- True
  , symInfoExpirationMode          :: Int    -- 15
  , symInfoFillingMode             :: Int    -- 1
  , symInfoOrderMode               :: Int    -- 119
  , symInfoOrderGtcMode            :: Int    -- 0
  , symInfoOptionMode              :: SymbolOptionMode    -- 0
  , symInfoOptionRight             :: SymbolOptionRight    -- 0
  , symInfoBid                     :: Double -- 408.1
  , symInfoBidhigh                 :: Double -- 408.56
  , symInfoBidlow                  :: Double -- 404.68
  , symInfoAsk                     :: Double -- 412.1
  , symInfoAskhigh                 :: Double -- 412.56
  , symInfoAsklow                  :: Double -- 408.68
  , symInfoLast                    :: Double -- 410.1
  , symInfoLasthigh                :: Double -- 410.56
  , symInfoLastlow                 :: Double -- 406.68
  , symInfoVolumeReal              :: Double -- 0.0
  , symInfoVolumehighReal          :: Double -- 0.0
  , symInfoVolumelowReal           :: Double -- 0.0
  , symInfoOptionStrike            :: Double -- 0.0
  , symInfoPoint                   :: Double -- 0.01
  , symInfoTradeTickValue          :: Double -- 2.4265954865323947
  , symInfoTradeTickValueProfit    :: Double -- 2.4265954865323947
  , symInfoTradeTickValueLoss      :: Double -- 2.450379808870375
  , symInfoTradeTickSize           :: Double -- 0.01
  , symInfoTradeContractSize       :: Double -- 100000.0
  , symInfoTradeAccruedInterest    :: Double -- 0.0
  , symInfoTradeFaceValue          :: Double -- 0.0
  , symInfoTradeLiquidityRate      :: Double -- 0.0
  , symInfoVolumeMin               :: Double -- 0.01
  , symInfoVolumeMax               :: Double -- 25.0
  , symInfoVolumeStep              :: Double -- 0.01
  , symInfoVolumeLimit             :: Double -- 60.0
  , symInfoSwapLong                :: Double -- 0.0
  , symInfoSwapShort               :: Double -- 0.0
  , symInfoMarginInitial           :: Double -- 0.0
  , symInfoMarginMaintenance       :: Double -- 0.0
  , symInfoSessionVolume           :: Double -- 0.0
  , symInfoSessionTurnover         :: Double -- 0.0
  , symInfoSessionInterest         :: Double -- 0.0
  , symInfoSessionBuyOrdersVolume  :: Double -- 0.0
  , symInfoSessionSellOrdersVolume :: Double -- 0.0
  , symInfoSessionOpen             :: Double -- 407.21
  , symInfoSessionClose            :: Double -- 407.02
  , symInfoSessionAw               :: Double -- 0.0
  , symInfoSessionPriceSettlement  :: Double -- 0.0
  , symInfoSessionPriceLimitMin    :: Double -- 0.0
  , symInfoSessionPriceLimitMax    :: Double -- 0.0
  , symInfoMarginHedged            :: Double -- 0.0
  , symInfoPriceChange             :: Double -- 0.7567
  , symInfoPriceVolatility         :: Double -- 0.0
  , symInfoPriceTheoretical        :: Double -- 0.0
  , symInfoPriceGreeksDelta        :: Double -- 0.0
  , symInfoPriceGreeksTheta        :: Double -- 0.0
  , symInfoPriceGreeksGamma        :: Double -- 0.0
  , symInfoPriceGreeksVega         :: Double -- 0.0
  , symInfoPriceGreeksRho          :: Double -- 0.0
  , symInfoPriceGreeksOmega        :: Double -- 0.0
  , symInfoPriceSensitivity        :: Double -- 0.0
  , symInfoBasis                   :: String -- ''
  , symInfoCategory                :: String -- 'EURHUF Curncy'
  , symInfoCurrencyBase            :: String -- 'EUR'
  , symInfoCurrencyProfit          :: String -- 'HUF'
  , symInfoCurrencyMargin          :: String -- 'EUR'
  , symInfoBank                    :: String -- ''
  , symInfoDescription             :: String -- 'EUR/HUF'
  , symInfoExchange                :: String -- ''
  , symInfoFormula                 :: String -- ''
  , symInfoIsin                    :: String -- ''
  , symInfoName                    :: String -- 'EURHUF'
  , symInfoPage                    :: String -- ''
  , symInfoPath                    :: String -- 'Forex\\EURHUF'
  } deriving (Show, Eq, Ord)

readSymbolInfo :: IO SymbolInfo
readSymbolInfo =
  SymbolInfo
    <$> (unpickle' "Bool" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (toSymbolCalcMode . unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (secondsToUTCTime . unpickle' "Int" <$> receive)
    <*> (secondsToUTCTime . unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Bool" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
    <*> (toEnum . unpickle' "Int" <$> receive)
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
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
    <*> (unpickle' "String" <$> receive)
