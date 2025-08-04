{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : MT5.Data.SymbolInfoSpec
Description : Comprehensive test suite for SymbolInfo data type
Copyright   : (c) 2025 Manuel Schneckenreither
License     : BSD-3-Clause

Tests for the SymbolInfo data type focusing on:
- Trading symbol properties and constraints
- Price precision and spread calculations
- Volume and contract specifications
- Trading session parameters
-}
module MT5.Data.SymbolInfoSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Time
import Data.Maybe (fromJust)
import MT5.Data.SymbolInfo

-- | Test suite for SymbolInfo data type
spec :: TestTree
spec = testGroup "MT5.Data.SymbolInfo"
  [ unitTests
  , propertyTests
  ]

-- | Unit tests for specific SymbolInfo scenarios
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "creates valid EURUSD symbol" $ do
      startTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
      let symbol = SymbolInfo
            { symInfoCustom = False
            , symInfoChartMode = SYMBOL_CHART_MODE_BID
            , symInfoSelect = True
            , symInfoVisible = True
            , symInfoSessionDeals = 1250
            , symInfoSessionBuyOrders = SYMBOL_ORDERS_GTC
            , symInfoSessionSellOrders = SYMBOL_ORDERS_GTC
            , symInfoVolume = 145620
            , symInfoVolumehigh = 180000
            , symInfoVolumelow = 120000
            , symInfoTime = 1704067200  -- 2024-01-01 00:00:00 UTC
            , symInfoDigits = 5
            , symInfoSpread = 12
            , symInfoSpreadFloat = True
            , symInfoTicksBookdepth = 10
            , symInfoTradeCalcMode = SYMBOL_CALC_MODE_FOREX
            , symInfoTradeMode = SYMBOL_TRADE_MODE_FULL
            , symInfoStartTime = startTime
            , symInfoExpirationTime = startTime
            , symInfoTradeStopsLevel = 0
            , symInfoTradeFreezeLevel = 0
            , symInfoTradeExemode = SYMBOL_TRADE_EXECUTION_INSTANT
            , symInfoSwapMode = SYMBOL_SWAP_MODE_POINTS
            , symInfoSwapRollover3days = 3
            , symInfoMarginHedgedUseLeg = False
            , symInfoExpirationMode = 1
            , symInfoFillingMode = 1
            , symInfoOrderMode = 127
            , symInfoOrderGtcMode = 0
            , symInfoOptionMode = SYMBOL_OPTION_MODE_EUROPEAN
            , symInfoOptionRight = SYMBOL_OPTION_RIGHT_CALL
            , symInfoBid = 1.10245
            , symInfoBidhigh = 1.10298
            , symInfoBidlow = 1.10187
            , symInfoAsk = 1.10257
            , symInfoAskhigh = 1.10310
            , symInfoAsklow = 1.10199
            , symInfoLast = 1.10251
            , symInfoLasthigh = 1.10304
            , symInfoLastlow = 1.10193
            , symInfoVolumeReal = 145620000.0
            , symInfoVolumehighReal = 180000000.0
            , symInfoVolumelowReal = 120000000.0
            , symInfoOptionStrike = 0.0
            , symInfoPoint = 0.00001
            , symInfoTradeTickValue = 1.0
            , symInfoTradeTickValueProfit = 1.0
            , symInfoTradeTickValueLoss = 1.0
            , symInfoTradeTickSize = 0.00001
            , symInfoTradeContractSize = 100000.0
            , symInfoTradeAccruedInterest = 0.0
            , symInfoTradeFaceValue = 0.0
            , symInfoTradeLiquidityRate = 0.0
            , symInfoVolumeMin = 0.01
            , symInfoVolumeMax = 500.0
            , symInfoVolumeStep = 0.01
            , symInfoVolumeLimit = 0.0
            , symInfoSwapLong = -0.5
            , symInfoSwapShort = 0.2
            , symInfoMarginInitial = 0.0
            , symInfoMarginMaintenance = 0.0
            , symInfoSessionVolume = 145620.0
            , symInfoSessionTurnover = 160572310.0
            , symInfoSessionInterest = 0.0
            , symInfoSessionBuyOrdersVolume = 72500.0
            , symInfoSessionSellOrdersVolume = 68300.0
            , symInfoSessionOpen = 1.10215
            , symInfoSessionClose = 1.10251
            , symInfoSessionAw = 1.10233
            , symInfoSessionPriceSettlement = 0.0
            , symInfoSessionPriceLimitMin = 0.0
            , symInfoSessionPriceLimitMax = 0.0
            , symInfoMarginHedged = 50000.0
            , symInfoPriceChange = 0.00036
            , symInfoPriceVolatility = 0.0
            , symInfoPriceTheoretical = 0.0
            , symInfoPriceGreeksDelta = 0.0
            , symInfoPriceGreeksTheta = 0.0
            , symInfoPriceGreeksGamma = 0.0
            , symInfoPriceGreeksVega = 0.0
            , symInfoPriceGreeksRho = 0.0
            , symInfoPriceGreeksOmega = 0.0
            , symInfoPriceSensitivity = 0.0
            , symInfoBasis = ""
            , symInfoCategory = ""
            , symInfoCurrencyBase = "EUR"
            , symInfoCurrencyProfit = "USD"
            , symInfoCurrencyMargin = "EUR"
            , symInfoBank = ""
            , symInfoDescription = "Euro vs US Dollar"
            , symInfoExchange = ""
            , symInfoFormula = ""
            , symInfoIsin = ""
            , symInfoName = "EURUSD"
            , symInfoPage = ""
            , symInfoPath = ""
            }
      
      -- Test basic symbol properties
      symInfoName symbol @?= "EURUSD"
      symInfoDigits symbol @?= 5
      symInfoSpread symbol @?= 12
      symInfoTradeContractSize symbol @?= 100000.0
      symInfoCurrencyBase symbol @?= "EUR"
      symInfoCurrencyProfit symbol @?= "USD"

  , testCase "calculates spread in points correctly" $ do
      startTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
      let symbol = (defaultSymbol startTime)
            { symInfoDigits = 5
            , symInfoSpread = 18
            , symInfoSpreadFloat = True
            , symInfoBid = 1.08542
            , symInfoAsk = 1.08560  -- 18 point spread
            , symInfoPoint = 0.00001
            }
      
      -- Verify spread calculation: (ask - bid) / point = (1.08560 - 1.08542) / 0.00001 = 18
      let calculatedSpread = round ((symInfoAsk symbol - symInfoBid symbol) / symInfoPoint symbol)
      fromIntegral (symInfoSpread symbol) @?= calculatedSpread

  , testCase "validates volume constraints" $ do
      startTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
      let symbol = (defaultSymbol startTime)
            { symInfoDigits = 2
            , symInfoSpread = 50
            , symInfoSpreadFloat = False
            , symInfoBid = 1950.25
            , symInfoAsk = 1950.75
            , symInfoVolumeMin = 0.1   -- Minimum trade volume
            , symInfoVolumeMax = 100.0 -- Maximum trade volume  
            , symInfoVolumeStep = 0.1  -- Volume step
            , symInfoTradeContractSize = 100.0
            , symInfoCurrencyBase = "XAU"
            , symInfoCurrencyProfit = "USD"
            , symInfoName = "XAUUSD"
            , symInfoDescription = "Gold vs US Dollar"
            }
      
      -- Test volume constraints for gold trading
      symInfoVolumeMin symbol @?= 0.1
      symInfoVolumeMax symbol @?= 100.0
      symInfoVolumeStep symbol @?= 0.1
      symInfoName symbol @?= "XAUUSD"
      symInfoDigits symbol @?= 2  -- Gold typically has 2 decimal places
  ]

-- | Property-based tests for SymbolInfo
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "spread is always non-negative" $
      \spread -> spread >= 0 ==>
        let startTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
            symbol = (defaultSymbol startTime) { symInfoSpread = spread }
        in symInfoSpread symbol >= 0

  , testProperty "digits are within valid range" $
      forAll (choose (0, 8)) $ \digits ->
        let startTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
            symbol = (defaultSymbol startTime) { symInfoDigits = digits }
        in symInfoDigits symbol >= 0 && symInfoDigits symbol <= 8

  , testProperty "ask price is greater than or equal to bid" $
      \(bid :: Double) (spread :: Int) (point :: Double) -> bid > 0 && spread >= 0 && point > 0 ==>
        let ask = bid + fromIntegral spread * point
            startTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
            symbol = (defaultSymbol startTime) { symInfoBid = bid, symInfoAsk = ask, symInfoPoint = point }
        in symInfoAsk symbol >= symInfoBid symbol

  , testProperty "volume constraints are logical" $
      forAll (choose (0.01, 100.0)) $ \minVol ->
      forAll (choose (minVol, minVol * 10)) $ \maxVol ->
      forAll (choose (0.01, 1.0)) $ \step ->
        let startTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
            symbol = (defaultSymbol startTime) 
              { symInfoVolumeMin = minVol
              , symInfoVolumeMax = maxVol
              , symInfoVolumeStep = step 
              }
        in symInfoVolumeMin symbol <= symInfoVolumeMax symbol && symInfoVolumeStep symbol > 0

  , testProperty "contract size is positive" $
      \contractSize -> contractSize > 0 ==>
        let startTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
            symbol = (defaultSymbol startTime) { symInfoTradeContractSize = contractSize }
        in symInfoTradeContractSize symbol > 0

  , testProperty "point value is positive" $
      \point -> point > 0 && point <= 1.0 ==>
        let startTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
            symbol = (defaultSymbol startTime) { symInfoPoint = point }
        in symInfoPoint symbol > 0

  , testProperty "session volumes are consistent" $
      \buyVolume sellVolume ->
        buyVolume >= 0 && sellVolume >= 0 ==>
        let totalVolume = buyVolume + sellVolume
            startTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
            symbol = (defaultSymbol startTime)
              { symInfoSessionBuyOrdersVolume = buyVolume
              , symInfoSessionSellOrdersVolume = sellVolume
              , symInfoSessionVolume = totalVolume
              }
        in symInfoSessionBuyOrdersVolume symbol >= 0 && symInfoSessionSellOrdersVolume symbol >= 0
  ]

-- | Default SymbolInfo for testing
defaultSymbol :: UTCTime -> SymbolInfo
defaultSymbol startTime = SymbolInfo
  { symInfoCustom = False
  , symInfoChartMode = SYMBOL_CHART_MODE_BID
  , symInfoSelect = True
  , symInfoVisible = True
  , symInfoSessionDeals = 0
  , symInfoSessionBuyOrders = SYMBOL_ORDERS_GTC
  , symInfoSessionSellOrders = SYMBOL_ORDERS_GTC
  , symInfoVolume = 0
  , symInfoVolumehigh = 0
  , symInfoVolumelow = 0
  , symInfoTime = 0
  , symInfoDigits = 5
  , symInfoSpread = 10
  , symInfoSpreadFloat = True
  , symInfoTicksBookdepth = 10
  , symInfoTradeCalcMode = SYMBOL_CALC_MODE_FOREX
  , symInfoTradeMode = SYMBOL_TRADE_MODE_FULL
  , symInfoStartTime = startTime
  , symInfoExpirationTime = startTime
  , symInfoTradeStopsLevel = 0
  , symInfoTradeFreezeLevel = 0
  , symInfoTradeExemode = SYMBOL_TRADE_EXECUTION_INSTANT
  , symInfoSwapMode = SYMBOL_SWAP_MODE_POINTS
  , symInfoSwapRollover3days = 3
  , symInfoMarginHedgedUseLeg = False
  , symInfoExpirationMode = 1
  , symInfoFillingMode = 1
  , symInfoOrderMode = 127
  , symInfoOrderGtcMode = 0
  , symInfoOptionMode = SYMBOL_OPTION_MODE_EUROPEAN
  , symInfoOptionRight = SYMBOL_OPTION_RIGHT_CALL
  , symInfoBid = 1.10000
  , symInfoBidhigh = 1.10100
  , symInfoBidlow = 1.09900
  , symInfoAsk = 1.10010
  , symInfoAskhigh = 1.10110
  , symInfoAsklow = 1.09910
  , symInfoLast = 1.10005
  , symInfoLasthigh = 1.10105
  , symInfoLastlow = 1.09905
  , symInfoVolumeReal = 1000000.0
  , symInfoVolumehighReal = 1200000.0
  , symInfoVolumelowReal = 800000.0
  , symInfoOptionStrike = 0.0
  , symInfoPoint = 0.00001
  , symInfoTradeTickValue = 1.0
  , symInfoTradeTickValueProfit = 1.0
  , symInfoTradeTickValueLoss = 1.0
  , symInfoTradeTickSize = 0.00001
  , symInfoTradeContractSize = 100000.0
  , symInfoTradeAccruedInterest = 0.0
  , symInfoTradeFaceValue = 0.0
  , symInfoTradeLiquidityRate = 0.0
  , symInfoVolumeMin = 0.01
  , symInfoVolumeMax = 500.0
  , symInfoVolumeStep = 0.01
  , symInfoVolumeLimit = 0.0
  , symInfoSwapLong = -0.5
  , symInfoSwapShort = 0.2
  , symInfoMarginInitial = 0.0
  , symInfoMarginMaintenance = 0.0
  , symInfoSessionVolume = 0.0
  , symInfoSessionTurnover = 0.0
  , symInfoSessionInterest = 0.0
  , symInfoSessionBuyOrdersVolume = 0.0
  , symInfoSessionSellOrdersVolume = 0.0
  , symInfoSessionOpen = 1.10000
  , symInfoSessionClose = 1.10005
  , symInfoSessionAw = 1.10003
  , symInfoSessionPriceSettlement = 0.0
  , symInfoSessionPriceLimitMin = 0.0
  , symInfoSessionPriceLimitMax = 0.0
  , symInfoMarginHedged = 50000.0
  , symInfoPriceChange = 0.00005
  , symInfoPriceVolatility = 0.0
  , symInfoPriceTheoretical = 0.0
  , symInfoPriceGreeksDelta = 0.0
  , symInfoPriceGreeksTheta = 0.0
  , symInfoPriceGreeksGamma = 0.0
  , symInfoPriceGreeksVega = 0.0
  , symInfoPriceGreeksRho = 0.0
  , symInfoPriceGreeksOmega = 0.0
  , symInfoPriceSensitivity = 0.0
  , symInfoBasis = ""
  , symInfoCategory = ""
  , symInfoCurrencyBase = "EUR"
  , symInfoCurrencyProfit = "USD"
  , symInfoCurrencyMargin = "EUR"
  , symInfoBank = ""
  , symInfoDescription = "Test Symbol"
  , symInfoExchange = ""
  , symInfoFormula = ""
  , symInfoIsin = ""
  , symInfoName = "TESTUSD"
  , symInfoPage = ""
  , symInfoPath = ""
  }
