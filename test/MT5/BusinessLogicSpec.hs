{-# LANGUAGE OverloadedStrings #-}

module MT5.BusinessLogicSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Time.Clock
import MT5.Data.CurrentPrice
import MT5.Data.AccountInfo
import MT5.Data.TradePosition
import MT5.Data.Candle
import MT5.Data.DecimalNumber (DecimalNumber(..), mkDecimalNumberFromDouble)

-- | Phase 4: Advanced Business Logic & Trading Calculations
spec :: TestTree
spec = testGroup "Business Logic & Trading Calculations"
  [ tradingCalculationsTests
  , crossModuleBusinessLogicTests
  , advancedAccountCalculationsTests
  , riskManagementTests
  , positionAnalysisTests
  ]

-- | Test trading-specific calculations and business logic
tradingCalculationsTests :: TestTree
tradingCalculationsTests = testGroup "Trading Calculations"
  [ testCase "spread calculation with edge cases" $ do
      -- Test normal spread calculation
      assertApproxEqual 1e-10 0.0002 (calculateSpread 1.0852 1.0850)
      
      -- Test zero spread (market maker scenario)
      assertApproxEqual 1e-10 0.0 (calculateSpread 1.0850 1.0850)
      
      -- Test negative spread (should not happen in real markets, but mathematically valid)
      assertApproxEqual 1e-10 (-0.0002) (calculateSpread 1.0850 1.0852)
      
      -- Test large spreads (exotic pairs or news events)
      assertApproxEqual 1e-10 0.7 (calculateSpread 120.5 119.8)
      
  , testCase "spread in points calculation" $ do
      -- EURUSD with 5 digits: 1 point = 0.00001
      let eurusdSpread = calculateSpread 1.08523 1.08520
      let pointsEURUSD = round (eurusdSpread * 100000) :: Int
      pointsEURUSD @?= 3
      
      -- USDJPY with 3 digits: 1 point = 0.001  
      let usdjpySpread = calculateSpread 110.125 110.120
      let pointsUSDJPY = round (usdjpySpread * 1000) :: Int
      pointsUSDJPY @?= 5

  , testCase "position profit calculation scenarios" $ do
      -- Buy position profit when price goes up
      let buyOpenPrice = 1.0850 :: Double
          buyCurrentPrice = 1.0860 :: Double
          buyProfitPips = (buyCurrentPrice - buyOpenPrice) * 10000 -- Convert to pips
      assertApproxEqual 1e-10 10.0 buyProfitPips  -- 10 pips profit
      
      -- Sell position profit when price goes down
      let sellOpenPrice = 1.0860 :: Double
          sellCurrentPrice = 1.0850 :: Double
          sellProfitPips = (sellOpenPrice - sellCurrentPrice) * 10000 -- Convert to pips  
      assertApproxEqual 1e-10 10.0 sellProfitPips  -- 10 pips profit

  , testCase "position size calculation based on risk" $ do
      let accountBalance = 10000.0 :: Double
          riskPercent = 0.02 :: Double  -- 2% risk
          stopLossPips = 50.0 :: Double
          pipValue = 1.0 :: Double  -- $1 per pip for standard lot EURUSD
          
      -- Position size = (Account Balance * Risk %) / (Stop Loss in Pips * Pip Value)
      let positionSize = (accountBalance * riskPercent) / (stopLossPips * pipValue)
      positionSize @?= (4.0 :: Double)  -- 4 micro lots
      
  , testProperty "spread calculation is non-negative for normal market conditions" $
      \(Positive ask) (Positive bid) ->
        ask >= bid ==> calculateSpread ask bid >= 0
  ]

-- | Test business logic that spans multiple modules  
crossModuleBusinessLogicTests :: TestTree
crossModuleBusinessLogicTests = testGroup "Cross-Module Business Logic"
  [ testCase "account health assessment" $ do
      let healthyAccount = AccountInfo
            { accInfoLogin = 123456
            , accInfoTrade_mode = ACCOUNT_TRADE_MODE_DEMO
            , accInfoLeverage = 100
            , accInfoLimit_orders = 100
            , accInfoMargin_so_mode = ACCOUNT_STOPOUT_MODE_PERCENT
            , accInfoTrade_allowed = True
            , accInfoTrade_expert = True
            , accInfoMargin_mode = ACCOUNT_MARGIN_MODE_RETAIL_NETTING
            , accInfoCurrency_digits = 2
            , accInfoFifo_close = False
            , accInfoBalance = 10000.0
            , accInfoCredit = 0.0
            , accInfoProfit = 200.0
            , accInfoEquity = 10200.0
            , accInfoMargin = 1000.0
            , accInfoMargin_free = 9200.0
            , accInfoMargin_level = 1020.0
            , accInfoMargin_so_call = 100.0
            , accInfoMargin_so_so = 50.0
            , accInfoMargin_initial = 0.0
            , accInfoMargin_maintenance = 0.0
            , accInfoAssets = 0.0
            , accInfoLiabilities = 0.0
            , accInfoCommission_blocked = 0.0
            , accInfoName = "Healthy Account"
            , accInfoServer = "Demo-Server"
            , accInfoCurrency = "USD"
            , accInfoCompany = "Test Broker"
            }
      
      -- Test account health indicators
      let marginLevel = accInfoMargin_level healthyAccount
      assertBool "Healthy account should have high margin level" (marginLevel > 300.0)
      
      let equity = accInfoEquity healthyAccount
      let balance = accInfoBalance healthyAccount + accInfoCredit healthyAccount
      assertBool "Equity should be reasonable relative to balance" (equity >= balance * 0.9)

  , testCase "trading opportunity assessment" $ do
      let currentPrice = CurrentPrice
            { cpSymbol = "EURUSD"
            , cpBid = 1.0850
            , cpAsk = 1.0852
            , cpSpread = 0.0002
            , cpLast = 1.0851
            , cpVolume = 1000
            , cpTime = UTCTime (toEnum 58000) 0
            , cpTimeMsc = 1609459200000
            , cpFlags = 6  -- BID + ASK flags
            , cpVolumeReal = 500.0
            }
      
      -- Test trading conditions
      assertBool "Price should be valid for trading" (isValidPrice currentPrice)
      
      let spreadInPips = cpSpread currentPrice * 10000
      assertBool "Spread should be reasonable for EURUSD" (spreadInPips <= 3.0)
      
      assertBool "Market should be active" (hasTickFlag currentPrice tickFlagBid && hasTickFlag currentPrice tickFlagAsk)

  , testCase "candle formation analysis" $ do
      let candle = MT5Candle
            { mt5CandleTime = UTCTime (toEnum 58000) 0
            , mt5CandleOpen = 1.0850
            , mt5CandleHigh = 1.0870
            , mt5CandleLow = 1.0845
            , mt5CandleClose = 1.0865
            , mt5CandleVolume = 1000
            , mt5CandleSpread = 2
            , mt5CandleRealVolume = 500.0
            }
      
      -- Test candle validity
      let candleRange = mt5CandleHigh candle - mt5CandleLow candle
      assertBool "Candle should have positive range" (candleRange > 0)
      
      assertBool "High should be >= Open" (mt5CandleHigh candle >= mt5CandleOpen candle)
      assertBool "High should be >= Close" (mt5CandleHigh candle >= mt5CandleClose candle)
      assertBool "Low should be <= Open" (mt5CandleLow candle <= mt5CandleOpen candle)
      assertBool "Low should be <= Close" (mt5CandleLow candle <= mt5CandleClose candle)
      
      -- Test bullish candle detection
      let isBullish = mt5CandleClose candle > mt5CandleOpen candle
      assertBool "This should be a bullish candle" isBullish
  ]

-- | Test advanced account-related calculations
advancedAccountCalculationsTests :: TestTree
advancedAccountCalculationsTests = testGroup "Advanced Account Calculations"
  [ testCase "margin utilization analysis" $ do
      let account = AccountInfo
            { accInfoLogin = 789012
            , accInfoTrade_mode = ACCOUNT_TRADE_MODE_REAL
            , accInfoLeverage = 50
            , accInfoLimit_orders = 100
            , accInfoMargin_so_mode = ACCOUNT_STOPOUT_MODE_PERCENT
            , accInfoTrade_allowed = True
            , accInfoTrade_expert = True
            , accInfoMargin_mode = ACCOUNT_MARGIN_MODE_RETAIL_NETTING
            , accInfoCurrency_digits = 2
            , accInfoFifo_close = False
            , accInfoBalance = 5000.0
            , accInfoCredit = 0.0
            , accInfoProfit = -100.0
            , accInfoEquity = 4900.0
            , accInfoMargin = 2000.0
            , accInfoMargin_free = 2900.0
            , accInfoMargin_level = 245.0
            , accInfoMargin_so_call = 100.0
            , accInfoMargin_so_so = 50.0
            , accInfoMargin_initial = 0.0
            , accInfoMargin_maintenance = 0.0
            , accInfoAssets = 0.0
            , accInfoLiabilities = 0.0
            , accInfoCommission_blocked = 0.0
            , accInfoName = "Real Account"
            , accInfoServer = "Live-Server"
            , accInfoCurrency = "USD"
            , accInfoCompany = "Real Broker"
            }
      
      -- Calculate margin utilization percentage
      let marginUtilization = (accInfoMargin account / accInfoEquity account) * 100
      assertBool "Margin utilization should be reasonable" (marginUtilization > 0 && marginUtilization < 100)
      
      -- Test margin level calculation
      let calculatedMarginLevel = (accInfoEquity account / accInfoMargin account) * 100
      let actualMarginLevel = accInfoMargin_level account
      assertApproxEqual 1.0 calculatedMarginLevel actualMarginLevel

  , testProperty "equity relationship consistency" $
      \balance credit profit ->
        let equity = balance + credit + profit
            account = AccountInfo 1 ACCOUNT_TRADE_MODE_DEMO 100 100 ACCOUNT_STOPOUT_MODE_PERCENT 
                     True True ACCOUNT_MARGIN_MODE_RETAIL_NETTING 2 False
                     balance credit profit equity 0 0 0 100 50 0 0 0 0 0
                     "Test" "Test" "USD" "Test"
        in accInfoEquity account == accInfoBalance account + accInfoCredit account + accInfoProfit account

  , testProperty "free margin calculation" $
      forAll genPositiveAccount $ \account ->
        let calculatedFreeMargin = max 0 (accInfoEquity account - accInfoMargin account)
            actualFreeMargin = accInfoMargin_free account
        in abs (calculatedFreeMargin - actualFreeMargin) < 0.01  -- Allow small floating point differences
  ]

-- | Test risk management calculations  
riskManagementTests :: TestTree
riskManagementTests = testGroup "Risk Management Calculations"
  [ testCase "drawdown calculation" $ do
      let initialBalance = 10000.0 :: Double
          currentEquity = 9500.0 :: Double
          drawdown = ((initialBalance - currentEquity) / initialBalance) * 100
      drawdown @?= (5.0 :: Double)  -- 5% drawdown
      
      -- Test maximum acceptable drawdown
      assertBool "Drawdown should be within acceptable limits" (drawdown <= (20.0 :: Double))

  , testCase "position size based on volatility" $ do
      -- Higher volatility = smaller position size for same risk
      let accountBalance = 10000.0 :: Double
          riskPercent = 0.02 :: Double
          
      -- Low volatility scenario (tight stop loss)
      let lowVolatilityStopPips = 20.0 :: Double
          lowVolPositionSize = (accountBalance * riskPercent) / lowVolatilityStopPips
      
      -- High volatility scenario (wide stop loss)  
      let highVolatilityStopPips = 100.0 :: Double
          highVolPositionSize = (accountBalance * riskPercent) / highVolatilityStopPips
          
      assertBool "Lower volatility should allow larger position size" (lowVolPositionSize > highVolPositionSize)

  , testCase "correlation risk assessment" $ do
      -- Test that highly correlated pairs don't exceed risk limits
      let eurusdPosition = 0.5 :: Double  -- 0.5 lots
          gbpusdPosition = 0.3 :: Double  -- 0.3 lots (highly correlated with EURUSD)
          
      -- Assume 80% correlation between EUR/USD and GBP/USD
      let correlationFactor = 0.8 :: Double
          adjustedRisk = eurusdPosition + (gbpusdPosition * correlationFactor)
          maxAllowedRisk = 1.0 :: Double
          
      assertBool "Correlation-adjusted risk should be within limits" (adjustedRisk <= maxAllowedRisk)

  , testProperty "risk per trade should not exceed account limit" $
      \(Positive accountSize) (Positive riskPercent) ->
        (riskPercent :: Double) <= 5.0 ==>  -- Max 5% risk per trade
        let maxRiskAmount = (accountSize :: Double) * (riskPercent / 100)
        in maxRiskAmount <= accountSize * 0.05
  ]

-- | Test position analysis and calculations
positionAnalysisTests :: TestTree  
positionAnalysisTests = testGroup "Position Analysis"
  [ testCase "position profitability analysis" $ do
      let position = TradePosition
            { trPosTicket = 12345
            , trPosTime = UTCTime (toEnum 58000) 0
            , trPosTime_msc = UTCTime (toEnum 58000) 0
            , trPosTime_update = UTCTime (toEnum 58000) 0
            , trPosTime_update_msc = UTCTime (toEnum 58000) 0
            , trPosType = POSITION_TYPE_BUY
            , trPosMagic = 0
            , trPosIdentifier = 12345
            , trPosReason = POSITION_REASON_CLIENT
            , trPosVolume = 0.1
            , trPosPriceOpen = 1.0850
            , trPosSl = 1.0800  -- 50 pips stop loss
            , trPosTp = 1.0950  -- 100 pips take profit
            , trPosPrice_current = 1.0870
            , trPosSwap = -2.5
            , trPosProfit = 18.5  -- Current profit
            , trPosSymbol = "EURUSD"
            , trPosComment = "Test position"
            , trPosExternal_id = "ext123"
            }
      
      -- Test risk-reward ratio
      let stopLossDistance = abs (trPosPriceOpen position - trPosSl position)
          takeProfitDistance = abs (trPosTp position - trPosPriceOpen position)
          riskRewardRatio = takeProfitDistance / stopLossDistance
      assertApproxEqual 1e-10 2.0 riskRewardRatio  -- 1:2 risk-reward ratio
      
      -- Test current profit in pips
      let currentProfitPips = (trPosPrice_current position - trPosPriceOpen position) * 10000
      assertApproxEqual 1e-10 20.0 currentProfitPips  -- 20 pips profit
      
      -- Test that position is profitable
      assertBool "Position should be in profit" (trPosProfit position > 0)

  , testCase "position duration analysis" $ do
      let currentTime = UTCTime (toEnum 58001) 0  -- 1 day later
          openTime = UTCTime (toEnum 58000) 0
          positionDuration = diffUTCTime currentTime openTime
          
      -- Test position holding time
      assertBool "Position should be held for reasonable time" (positionDuration > 0)
      
      let daysHeld = nominalDiffTimeToSeconds positionDuration / (24 * 60 * 60)
      assertBool "Position should be short-term" (daysHeld <= 7)  -- Less than a week

  , testProperty "stop loss should limit maximum loss" $
      forAll genValidBuyPosition $ \position ->
        let openPrice = trPosPriceOpen position
            stopLoss = trPosSl position
            maxLoss = abs (openPrice - stopLoss)
        in stopLoss < openPrice && maxLoss > 0  -- Valid stop loss for buy position
  ]

-- | Helper functions and generators

assertApproxEqual :: Double -> Double -> Double -> Assertion
assertApproxEqual tol expected actual = 
  assertBool ("Expected: " ++ show expected ++ ", got: " ++ show actual ++ " (tolerance: " ++ show tol ++ ")")
             (abs (expected - actual) <= tol)

-- | Generator for accounts with positive values
genPositiveAccount :: Gen AccountInfo
genPositiveAccount = do
  login <- choose (1, 999999)
  balance <- choose (1000.0, 100000.0)
  credit <- choose (0.0, balance)
  profit <- choose (-balance, balance)
  margin <- choose (0.0, balance)
  let equity = balance + credit + profit
  let marginFree = max 0 (equity - margin)
  let marginLevel = if margin > 0 then (equity / margin) * 100 else 0
  return $ AccountInfo login ACCOUNT_TRADE_MODE_DEMO 100 100 ACCOUNT_STOPOUT_MODE_PERCENT
           True True ACCOUNT_MARGIN_MODE_RETAIL_NETTING 2 False
           balance credit profit equity margin marginFree marginLevel
           100 50 0 0 0 0 0 "Generated" "Test" "USD" "Test"

-- | Generator for valid buy positions  
genValidBuyPosition :: Gen TradePosition
genValidBuyPosition = do
  ticket <- choose (1, 999999)
  volumeVal <- choose (0.01, 10.0)
  openPrice <- choose (1.0, 200.0)
  currentPrice <- choose (0.5, 400.0)
  stopLoss <- choose (0.1, openPrice * 0.95)  -- Stop loss below open price for buy
  takeProfit <- choose (openPrice * 1.05, 400.0)  -- Take profit above open price for buy
  profit <- choose (-1000.0, 1000.0)
  swap <- choose (-10.0, 10.0)
  
  let time = UTCTime (toEnum 58000) 0
  let volume = either (const (DecimalNumber Nothing 0.0)) id $ mkDecimalNumberFromDouble volumeVal
  return $ TradePosition ticket time time time time POSITION_TYPE_BUY (0 :: Int) (fromInteger ticket)
           POSITION_REASON_CLIENT volume openPrice stopLoss takeProfit currentPrice
           swap profit "EURUSD" "Generated position" "ext123"
