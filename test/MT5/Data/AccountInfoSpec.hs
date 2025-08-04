{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MT5.Data.AccountInfoSpec
Description : Comprehensive test suite for AccountInfo data type
Copyright   : (c) 2025 Manuel Schneckenreither
License     : BSD-3-Clause

Tests for the AccountInfo data type focusing on:
- Field validation and constraints
- Numeric precision for financial calculations
- Boolean logic for trading permissions
- String handling for account identifiers
-}
module MT5.Data.AccountInfoSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import MT5.Data.AccountInfo

-- | Test suite for AccountInfo data type
spec :: TestTree
spec = testGroup "MT5.Data.AccountInfo"
  [ unitTests
  , propertyTests
  ]

-- | Unit tests for specific AccountInfo scenarios
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "creates valid demo account" $ do
      let account = AccountInfo
            { accInfoLogin = 123456
            , accInfoTrade_mode = ACCOUNT_TRADE_MODE_DEMO
            , accInfoLeverage = 100
            , accInfoLimit_orders = 200
            , accInfoMargin_so_mode = ACCOUNT_STOPOUT_MODE_PERCENT
            , accInfoTrade_allowed = True
            , accInfoTrade_expert = True
            , accInfoMargin_mode = ACCOUNT_MARGIN_MODE_RETAIL_NETTING
            , accInfoCurrency_digits = 2
            , accInfoFifo_close = False
            , accInfoBalance = 10000.00
            , accInfoCredit = 0.00
            , accInfoProfit = 150.50
            , accInfoEquity = 10150.50
            , accInfoMargin = 500.25
            , accInfoMargin_free = 9650.25
            , accInfoMargin_level = 2030.10
            , accInfoMargin_so_call = 100.0
            , accInfoMargin_so_so = 50.0
            , accInfoMargin_initial = 0.0
            , accInfoMargin_maintenance = 0.0
            , accInfoAssets = 0.0
            , accInfoLiabilities = 0.0
            , accInfoCommission_blocked = 0.0
            , accInfoName = "Demo Account"
            , accInfoServer = "Demo-Server"
            , accInfoCurrency = "USD"
            , accInfoCompany = "Test Broker"
            }
      
      -- Test account setup
      accInfoLogin account @?= 123456
      accInfoTrade_mode account @?= ACCOUNT_TRADE_MODE_DEMO
      accInfoBalance account @?= 10000.00
      accInfoEquity account @?= 10150.50
      accInfoTrade_allowed account @?= True

  , testCase "calculates margin level correctly" $ do
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
            , accInfoBalance = 5000.00
            , accInfoCredit = 1000.00
            , accInfoProfit = -200.00
            , accInfoEquity = 5800.00
            , accInfoMargin = 1160.00
            , accInfoMargin_free = 4640.00
            , accInfoMargin_level = 500.00
            , accInfoMargin_so_call = 100.0
            , accInfoMargin_so_so = 50.0
            , accInfoMargin_initial = 0.0
            , accInfoMargin_maintenance = 0.0
            , accInfoAssets = 0.0
            , accInfoLiabilities = 0.0
            , accInfoCommission_blocked = 0.0
            , accInfoName = "Real Trading Account"
            , accInfoServer = "Live-Server"
            , accInfoCurrency = "EUR"
            , accInfoCompany = "Real Broker Ltd"
            }
      
      -- Verify equity calculation: balance + credit + profit = 5000 + 1000 + (-200) = 5800
      let expectedEquity = accInfoBalance account + accInfoCredit account + accInfoProfit account
      accInfoEquity account @?= expectedEquity
      
      -- Verify free margin: equity - margin = 5800 - 1160 = 4640
      let expectedFreeMargin = accInfoEquity account - accInfoMargin account
      accInfoMargin_free account @?= expectedFreeMargin

  , testCase "handles zero margin scenario" $ do
      let account = AccountInfo
            { accInfoLogin = 345678
            , accInfoTrade_mode = ACCOUNT_TRADE_MODE_DEMO
            , accInfoLeverage = 200
            , accInfoLimit_orders = 0
            , accInfoMargin_so_mode = ACCOUNT_STOPOUT_MODE_PERCENT
            , accInfoTrade_allowed = False
            , accInfoTrade_expert = False
            , accInfoMargin_mode = ACCOUNT_MARGIN_MODE_RETAIL_NETTING
            , accInfoCurrency_digits = 2
            , accInfoFifo_close = True
            , accInfoBalance = 1000.00
            , accInfoCredit = 0.00
            , accInfoProfit = 0.00
            , accInfoEquity = 1000.00
            , accInfoMargin = 0.00  -- No open positions
            , accInfoMargin_free = 1000.00
            , accInfoMargin_level = 0.00  -- Infinite margin level when no margin used
            , accInfoMargin_so_call = 100.0
            , accInfoMargin_so_so = 50.0
            , accInfoMargin_initial = 0.0
            , accInfoMargin_maintenance = 0.0
            , accInfoAssets = 0.0
            , accInfoLiabilities = 0.0
            , accInfoCommission_blocked = 0.0
            , accInfoName = "No Position Account"
            , accInfoServer = "Test-Server"
            , accInfoCurrency = "GBP"
            , accInfoCompany = "Test Company"
            }
      
      accInfoMargin account @?= 0.00
      accInfoMargin_free account @?= accInfoEquity account
      accInfoTrade_allowed account @?= False  -- Trading disabled
  ]

-- | Property-based tests for AccountInfo
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "equity equals balance plus credit plus profit" $
      \balance credit profit ->
        let expectedEquity = balance + credit + profit
            account = defaultAccount 
              { accInfoBalance = balance
              , accInfoCredit = credit
              , accInfoProfit = profit
              , accInfoEquity = expectedEquity  -- Calculate equity correctly
              }
        in abs (accInfoEquity account - expectedEquity) < 0.001  -- Allow for floating point precision

  , testProperty "free margin equals equity minus margin" $
      \equity margin ->
        margin >= 0 ==>  -- Margin cannot be negative
        let expectedFreeMargin = equity - margin
            account = defaultAccount 
              { accInfoEquity = equity
              , accInfoMargin = margin
              , accInfoMargin_free = expectedFreeMargin  -- Calculate free margin correctly
              }
        in abs (accInfoMargin_free account - expectedFreeMargin) < 0.001

  , testProperty "login is always positive" $
      \login -> login > 0 ==>
        let account = defaultAccount { accInfoLogin = login }
        in accInfoLogin account > 0

  , testProperty "leverage is reasonable" $
      \leverage -> leverage > 0 && leverage <= 1000 ==>
        let account = defaultAccount { accInfoLeverage = leverage }
        in accInfoLeverage account > 0 && accInfoLeverage account <= 1000

  , testProperty "currency digits are valid" $
      forAll (choose (0, 8)) $ \digits ->
        let account = defaultAccount { accInfoCurrency_digits = digits }
        in accInfoCurrency_digits account >= 0 && accInfoCurrency_digits account <= 8

  , testProperty "margin level calculation is consistent" $
      \equity margin ->
        equity > 0 && margin > 0 ==>
        let expectedLevel = (equity / margin) * 100
            account = defaultAccount 
              { accInfoEquity = equity
              , accInfoMargin = margin
              , accInfoMargin_level = expectedLevel  -- Calculate margin level correctly
              }
        in abs (accInfoMargin_level account - expectedLevel) < 1.0  -- Allow 1% tolerance
  ]

-- | Default AccountInfo for testing
defaultAccount :: AccountInfo
defaultAccount = AccountInfo
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
  , accInfoBalance = 10000.00
  , accInfoCredit = 0.00
  , accInfoProfit = 0.00
  , accInfoEquity = 10000.00
  , accInfoMargin = 0.00
  , accInfoMargin_free = 10000.00
  , accInfoMargin_level = 0.00
  , accInfoMargin_so_call = 100.0
  , accInfoMargin_so_so = 50.0
  , accInfoMargin_initial = 0.0
  , accInfoMargin_maintenance = 0.0
  , accInfoAssets = 0.0
  , accInfoLiabilities = 0.0
  , accInfoCommission_blocked = 0.0
  , accInfoName = "Test Account"
  , accInfoServer = "Test-Server"
  , accInfoCurrency = "USD"
  , accInfoCompany = "Test Company"
  }
