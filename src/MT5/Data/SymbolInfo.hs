module MT5.Data.SymbolInfo
    ( SymbolInfo (..)
    , readSymbolInfo
    ) where


import           MT5.Communication

data SymbolInfo = SymbolInfo
  { symInfoCustom                  :: Bool   -- False
  , symInfoChartMode               :: Int    -- 1
  , symInfoSelect                  :: Bool   -- True
  , symInfoVisible                 :: Bool   -- False
  , symInfoSessionDeals            :: Int    -- 0
  , symInfoSessionBuyOrders        :: Int    -- 0
  , symInfoSessionSellOrders       :: Int    -- 0
  , symInfoVolume                  :: Int    -- 0
  , symInfoVolumehigh              :: Int    -- 0
  , symInfoVolumelow               :: Int    -- 0
  , symInfoTime                    :: Int    -- 1744412338
  , symInfoDigits                  :: Int    -- 2
  , symInfoSpread                  :: Int    -- 400
  , symInfoSpreadFloat             :: Bool   -- False
  , symInfoTicksBookdepth          :: Int    -- 0
  , symInfoTradeCalcMode           :: Int    -- 0
  , symInfoTradeMode               :: Int    -- 0
  , symInfoStartTime               :: Int    -- 0
  , symInfoExpirationTime          :: Int    -- 0
  , symInfoTradeStopsLevel         :: Int    -- 0
  , symInfoTradeFreezeLevel        :: Int    -- 0
  , symInfoTradeExemode            :: Int    -- 2
  , symInfoSwapMode                :: Int    -- 0
  , symInfoSwapRollover3days       :: Int    -- 3
  , symInfoMarginHedgedUseLeg      :: Bool   -- True
  , symInfoExpirationMode          :: Int    -- 15
  , symInfoFillingMode             :: Int    -- 1
  , symInfoOrderMode               :: Int    -- 119
  , symInfoOrderGtcMode            :: Int    -- 0
  , symInfoOptionMode              :: Int    -- 0
  , symInfoOptionRight             :: Int    -- 0
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
  } deriving (Show, Eq)

readSymbolInfo :: IO SymbolInfo
readSymbolInfo =
  SymbolInfo
  <$> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
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
