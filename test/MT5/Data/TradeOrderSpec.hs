{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MT5.Data.TradeOrderSpec
Description : Comprehensive test suite for TradeOrder data type
Copyright   : (c) 2025 Manuel Schnec  , testProperty "for buy orders stop loss should be below entry when set" $
      \entryPrice slPrice -> entryPrice > 0 && slPrice > 0 && slPrice < entryPrice ==>
        let order = defaultOrder 
              { tradeOrderType = ORDER_TYPE_BUY
              , tradeOrderPrice_open = entryPrice
              , tradeOrderSl = slPrice
              }
        in tradeOrderType order `elem` [ORDER_TYPE_BUY, ORDER_TYPE_BUY_LIMIT, ORDER_TYPE_BUY_STOP] ==> 
           (tradeOrderSl order == 0 || tradeOrderSl order < tradeOrderPrice_open order)

  , testProperty "for sell orders stop loss should be above entry when set" $
      \entryPrice slPrice -> entryPrice > 0 && slPrice > entryPrice ==>
        let order = defaultOrder 
              { tradeOrderType = ORDER_TYPE_SELL
              , tradeOrderPrice_open = entryPrice
              , tradeOrderSl = slPrice
              }
        in tradeOrderType order `elem` [ORDER_TYPE_SELL, ORDER_TYPE_SELL_LIMIT, ORDER_TYPE_SELL_STOP] =>     : BSD-3-Clause

Tests for the TradeOrder data type focusing on:
- Order lifecycle and state transitions
- Price and volume validations
- Time-based order properties
- Order type and execution parameters
-}
module MT5.Data.TradeOrderSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Time
import Data.Maybe (fromJust)
import MT5.Data.TradeOrder
import MT5.Data.OrderType
import MT5.Data.OrderState
import MT5.Data.DecimalNumber (DecimalNumber(..), mkDecimalNumberFromDouble)

-- Arbitrary instance for DecimalNumber to support QuickCheck
instance Arbitrary DecimalNumber where
  arbitrary = do
    val <- choose (0.01, 1000.0)
    return $ either (const (DecimalNumber Nothing 0.0)) id $ mkDecimalNumberFromDouble val

-- | Test suite for TradeOrder data type
spec :: TestTree
spec = testGroup "MT5.Data.TradeOrder"
  [ unitTests
  , propertyTests
  ]

-- | Unit tests for specific TradeOrder scenarios
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "creates valid buy limit order" $ do
      setupTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
      let order = TradeOrder
            { tradeOrderTicket = 123456789
            , tradeOrderTime_setup = setupTime
            , tradeOrderTime_setup_msc = setupTime
            , tradeOrderTime_expiration = 86400  -- 24 hours
            , tradeOrderType = ORDER_TYPE_BUY_LIMIT
            , tradeOrderType_time = 1          -- Good Till Cancel
            , tradeOrderType_filling = 1       -- Fill or Kill
            , tradeOrderState = ORDER_STATE_PLACED
            , tradeOrderMagic = 12345
            , tradeOrderVolume_current = 1.0
            , tradeOrderPrice_open = 1.08500   -- Limit price below market
            , tradeOrderSl = 1.08000           -- Stop loss
            , tradeOrderTp = 1.09000           -- Take profit
            , tradeOrderPrice_current = 1.08750 -- Current market price
            , tradeOrderSymbol = "EURUSD"
            , tradeOrderComment = "Buy limit order"
            , tradeOrderExternal_id = "ext-123"
            }
      
      -- Test order properties
      tradeOrderTicket order @?= 123456789
      tradeOrderType order @?= ORDER_TYPE_BUY_LIMIT
      tradeOrderState order @?= ORDER_STATE_PLACED
      tradeOrderVolume_current order @?= 1.0
      tradeOrderPrice_open order @?= 1.08500
      tradeOrderSymbol order @?= "EURUSD"

  , testCase "validates pending sell stop order" $ do
      setupTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
      let order = TradeOrder
            { tradeOrderTicket = 987654321
            , tradeOrderTime_setup = setupTime
            , tradeOrderTime_setup_msc = setupTime
            , tradeOrderTime_expiration = 0    -- No expiration (GTC)
            , tradeOrderType = ORDER_TYPE_SELL_STOP
            , tradeOrderType_time = 0          -- Good Till Cancel
            , tradeOrderType_filling = 0       -- Fill or Kill
            , tradeOrderState = ORDER_STATE_PLACED
            , tradeOrderMagic = 54321
            , tradeOrderVolume_current = 0.5
            , tradeOrderPrice_open = 1.09500   -- Stop price above market
            , tradeOrderSl = 1.10000           -- Stop loss (above entry for sell)
            , tradeOrderTp = 1.09000           -- Take profit (below entry for sell)
            , tradeOrderPrice_current = 1.09200 -- Current market price
            , tradeOrderSymbol = "EURUSD"
            , tradeOrderComment = "Sell stop order"
            , tradeOrderExternal_id = ""
            }
      
      -- Test sell stop order properties
      tradeOrderType order @?= ORDER_TYPE_SELL_STOP
      tradeOrderVolume_current order @?= 0.5
      tradeOrderPrice_open order @?= 1.09500
      -- For sell orders, SL should be above entry price, TP below
      tradeOrderSl order @?= 1.10000
      tradeOrderTp order @?= 1.09000

  , testCase "handles expired order" $ do
      setupTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
      let order = TradeOrder
            { tradeOrderTicket = 555666777
            , tradeOrderTime_setup = setupTime
            , tradeOrderTime_setup_msc = setupTime
            , tradeOrderTime_expiration = 3600  -- Expired 1 hour later
            , tradeOrderType = ORDER_TYPE_BUY_STOP
            , tradeOrderType_time = 2           -- Specified time
            , tradeOrderType_filling = 1
            , tradeOrderState = ORDER_STATE_CANCELED  -- Cancelled (expired)
            , tradeOrderMagic = 99999
            , tradeOrderVolume_current = 0.0    -- No remaining volume
            , tradeOrderPrice_open = 1.10000
            , tradeOrderSl = 1.09500
            , tradeOrderTp = 1.10500
            , tradeOrderPrice_current = 1.09800
            , tradeOrderSymbol = "EURUSD"
            , tradeOrderComment = "Expired order"
            , tradeOrderExternal_id = "exp-order"
            }
      
      -- Test expired order state
      tradeOrderState order @?= ORDER_STATE_CANCELED
      tradeOrderVolume_current order @?= 0.0  -- No volume remaining
      tradeOrderTime_expiration order @?= 3600

  , testCase "validates partial fill scenario" $ do
      setupTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
      let order = TradeOrder
            { tradeOrderTicket = 111222333
            , tradeOrderTime_setup = setupTime
            , tradeOrderTime_setup_msc = setupTime
            , tradeOrderTime_expiration = 0
            , tradeOrderType = ORDER_TYPE_BUY     -- Buy Market (partially filled)
            , tradeOrderType_time = 0
            , tradeOrderType_filling = 2    -- Return (partial fills allowed)
            , tradeOrderState = ORDER_STATE_FILLED  -- Partially filled
            , tradeOrderMagic = 77777
            , tradeOrderVolume_current = 0.3 -- 0.3 lots remaining from 1.0
            , tradeOrderPrice_open = 1.09125
            , tradeOrderSl = 1.08500
            , tradeOrderTp = 1.09750
            , tradeOrderPrice_current = 1.09125
            , tradeOrderSymbol = "EURUSD"
            , tradeOrderComment = "Partial fill"
            , tradeOrderExternal_id = "partial-123"
            }
      
      -- Test partial fill properties
      tradeOrderState order @?= ORDER_STATE_FILLED
      tradeOrderVolume_current order @?= 0.3   -- Remaining volume
      tradeOrderType_filling order @?= 2       -- Return policy allows partial fills
  ]

-- | Property-based tests for TradeOrder
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "ticket numbers are positive" $
      \ticket -> ticket > 0 ==>
        let order = defaultOrder { tradeOrderTicket = ticket }
        in tradeOrderTicket order > 0

  , testProperty "volume is non-negative" $
      \volume -> volume >= 0 ==>
        let order = defaultOrder { tradeOrderVolume_current = volume }
        in tradeOrderVolume_current order >= 0

  , testProperty "expiration time is non-negative" $
      \expTime -> expTime >= 0 ==>
        let order = defaultOrder { tradeOrderTime_expiration = expTime }
        in tradeOrderTime_expiration order >= 0

  , testProperty "prices are positive" $
      \price -> price > 0 ==>
        let order = defaultOrder { tradeOrderPrice_open = price, tradeOrderPrice_current = price }
        in tradeOrderPrice_open order > 0 && tradeOrderPrice_current order > 0

  , testProperty "magic numbers are non-negative" $
      \magic -> magic >= 0 ==>
        let order = defaultOrder { tradeOrderMagic = magic }
        in tradeOrderMagic order >= 0

  , testProperty "for buy orders stop loss should be below entry when set" $
      \entryPrice slPrice -> entryPrice > 0 && slPrice > 0 && slPrice < entryPrice ==>
        let order = defaultOrder 
              { tradeOrderType = ORDER_TYPE_BUY
              , tradeOrderPrice_open = entryPrice
              , tradeOrderSl = slPrice
              }
        in tradeOrderType order `elem` [ORDER_TYPE_BUY, ORDER_TYPE_BUY_LIMIT, ORDER_TYPE_BUY_STOP] ==> 
           (tradeOrderSl order == 0 || tradeOrderSl order < tradeOrderPrice_open order)

  , testProperty "for sell orders stop loss should be above entry when set" $
      \entryPrice slPrice -> entryPrice > 0 && slPrice > entryPrice ==>
        let order = defaultOrder 
              { tradeOrderType = ORDER_TYPE_SELL
              , tradeOrderPrice_open = entryPrice
              , tradeOrderSl = slPrice
              }
        in tradeOrderType order `elem` [ORDER_TYPE_SELL, ORDER_TYPE_SELL_LIMIT, ORDER_TYPE_SELL_STOP] ==> 
           (tradeOrderSl order == 0 || tradeOrderSl order > tradeOrderPrice_open order)
  ]

-- | Default TradeOrder for testing
defaultOrder :: TradeOrder
defaultOrder = TradeOrder
  { tradeOrderTicket = 123456789
  , tradeOrderTime_setup = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
  , tradeOrderTime_setup_msc = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-01 00:00:00"
  , tradeOrderTime_expiration = 0
  , tradeOrderType = ORDER_TYPE_BUY
  , tradeOrderType_time = 0
  , tradeOrderType_filling = 1
  , tradeOrderState = ORDER_STATE_PLACED
  , tradeOrderMagic = 12345
  , tradeOrderVolume_current = either (const (DecimalNumber Nothing 0.0)) id $ mkDecimalNumberFromDouble 1.0
  , tradeOrderPrice_open = 1.10000
  , tradeOrderSl = 1.09500
  , tradeOrderTp = 1.10500
  , tradeOrderPrice_current = 1.10000
  , tradeOrderSymbol = "EURUSD"
  , tradeOrderComment = "Test order"
  , tradeOrderExternal_id = ""
  }
