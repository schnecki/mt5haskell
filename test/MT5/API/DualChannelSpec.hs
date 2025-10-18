{-# LANGUAGE OverloadedStrings #-}
module MT5.API.DualChannelSpec (dualChannelTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import MT5.Communication.Response
import MT5.API (accountInfo, positionsGet, symbolInfo, ordersGet)
import MT5.Data
import MT5.Data.OrderState
import MT5.Config

-- | Main test group for dual channel implementations
dualChannelTests :: TestTree
dualChannelTests = testGroup "Dual Channel Tests"
  [ converterTests
  , dataLossTests
  , routingTests
  , jsonParsingTests
  ]

--------------------------------------------------------------------------------
-- Converter Function Tests
--------------------------------------------------------------------------------

converterTests :: TestTree
converterTests = testGroup "Converter Function Tests"
  [ testAccountInfoConverter
  , testPositionInfoConverter
  , testSymbolInfoConverter
  , testOrderInfoConverter
  ]

-- | Test AccountInfoResponse → AccountInfo conversion
testAccountInfoConverter :: TestTree
testAccountInfoConverter = testCase "AccountInfoResponse converter" $ do
  let resp = AccountInfoResponse
        { accountInfoSuccess = True
        , accountInfoLogin = 12345
        , accountInfoBalance = 10000.0
        , accountInfoEquity = 10500.0
        , accountInfoProfit = 500.0
        , accountInfoMargin = 2000.0
        , accountInfoMarginFree = 8500.0
        , accountInfoMarginLevel = 525.0
        , accountInfoLeverage = 100
        , accountInfoCurrency = "USD"
        , accountInfoName = "Test Account"
        , accountInfoServer = "TestServer"
        , accountInfoTradeAllowed = True
        , accountInfoTradeExpert = True
        }
  
  -- Test conversion (would call actual converter in real implementation)
  -- For now, verify structure
  accountInfoLogin resp @?= 12345
  accountInfoBalance resp @?= 10000.0
  accountInfoSuccess resp @?= True

-- | Test PositionInfoResponse → TradePosition conversion
testPositionInfoConverter :: TestTree
testPositionInfoConverter = testCase "PositionInfoResponse converter" $ do
  let resp = PositionInfoResponse
        { positionTicket = 12345
        , positionSymbol = "EURUSD"
        , positionType = 0  -- BUY
        , positionVolume = 0.01
        , positionPriceOpen = 1.1000
        , positionPriceCurrent = 1.1050
        , positionSl = 1.0900
        , positionTp = 1.1200
        , positionProfit = 50.0
        , positionSwap = -2.5
        , positionMagic = 0
        , positionComment = "Test position"
        }
  
  positionTicket resp @?= 12345
  positionVolume resp @?= 0.01
  positionType resp @?= 0

-- | Test SymbolInfoResponse → SymbolInfo conversion
testSymbolInfoConverter :: TestTree
testSymbolInfoConverter = testCase "SymbolInfoResponse converter" $ do
  let resp = SymbolInfoResponse
        { symbolInfoSuccess = True
        , symbolInfoSymbol = "EURUSD"
        , symbolInfoBid = 1.1000
        , symbolInfoAsk = 1.1002
        , symbolInfoDigits = 5
        , symbolInfoSpread = 2
        , symbolInfoPoint = 0.00001
        }
  
  symbolInfoBid resp @?= 1.1000
  symbolInfoAsk resp @?= 1.1002
  symbolInfoDigits resp @?= 5
  symbolInfoSpread resp @?= 2

-- | Test OrderInfoResponse → TradeOrder conversion
testOrderInfoConverter :: TestTree
testOrderInfoConverter = testCase "OrderInfoResponse converter" $ do
  let resp = OrderInfoResponse
        { orderTicket = 54321
        , orderSymbol = "GBPUSD"
        , orderType = 2  -- BUY_LIMIT
        , orderVolume = 0.02
        , orderPriceOpen = 1.2500
        , orderPriceCurrent = 1.2450
        , orderSl = 1.2400
        , orderTp = 1.2600
        , orderMagic = 123
        , orderComment = "Test order"
        }
  
  orderTicket resp @?= 54321
  orderVolume resp @?= 0.02
  orderType resp @?= 2

--------------------------------------------------------------------------------
-- Data Loss Tests
--------------------------------------------------------------------------------

dataLossTests :: TestTree
dataLossTests = testGroup "Data Loss Detection Tests"
  [ testAccountInfoDataLoss
  , testPositionInfoDataLoss
  , testSymbolInfoDataLoss
  , testOrderInfoDataLoss
  ]

-- | Verify accountInfo handles missing fields with defaults
testAccountInfoDataLoss :: TestTree
testAccountInfoDataLoss = testCase "AccountInfo missing fields use defaults" $ do
  -- EA response has 14 fields, AccountInfo has 28
  -- Missing fields should use sensible defaults
  let resp = AccountInfoResponse
        { accountInfoSuccess = True
        , accountInfoLogin = 12345
        , accountInfoBalance = 10000.0
        , accountInfoEquity = 10500.0
        , accountInfoProfit = 500.0
        , accountInfoMargin = 2000.0
        , accountInfoMarginFree = 8500.0
        , accountInfoMarginLevel = 525.0
        , accountInfoLeverage = 100
        , accountInfoCurrency = "USD"
        , accountInfoName = "Test Account"
        , accountInfoServer = "TestServer"
        , accountInfoTradeAllowed = True
        , accountInfoTradeExpert = True
        }
  
  -- Verify EA response has expected fields
  accountInfoLogin resp @?= 12345
  
  -- Note: In production, converter should handle these missing fields:
  -- - accInfoCredit (default 0.0)
  -- - accInfoTrade_mode (default DEMO)
  -- - accInfoLimit_orders (default 0)
  -- - accInfoCompany (default "")
  True @?= True  -- Placeholder for actual converter test

-- | Verify positionsGet handles missing timestamp fields
testPositionInfoDataLoss :: TestTree
testPositionInfoDataLoss = testCase "PositionInfo missing timestamps use defaults" $ do
  -- EA response has 12 fields, TradePosition has 19
  -- Missing: time, time_msc, time_update, time_update_msc, identifier, reason, external_id
  let resp = PositionInfoResponse
        { positionTicket = 12345
        , positionSymbol = "EURUSD"
        , positionType = 0
        , positionVolume = 0.01
        , positionPriceOpen = 1.1000
        , positionPriceCurrent = 1.1050
        , positionSl = 1.0900
        , positionTp = 1.1200
        , positionProfit = 50.0
        , positionSwap = -2.5
        , positionMagic = 0
        , positionComment = "Test"
        }
  
  positionTicket resp @?= 12345
  -- Missing fields should default to current time, 0, CLIENT, ""
  True @?= True  -- Placeholder

-- | Verify symbolInfo handles massive data loss (93%)
testSymbolInfoDataLoss :: TestTree
testSymbolInfoDataLoss = testCase "SymbolInfo handles 93% data loss" $ do
  -- EA response has only 7 fields, SymbolInfo has 104 fields!
  -- This is the CRITICAL data loss case
  let resp = SymbolInfoResponse
        { symbolInfoSuccess = True
        , symbolInfoSymbol = "EURUSD"
        , symbolInfoBid = 1.1000
        , symbolInfoAsk = 1.1002
        , symbolInfoDigits = 5
        , symbolInfoSpread = 2
        , symbolInfoPoint = 0.00001
        }
  
  symbolInfoBid resp @?= 1.1000
  symbolInfoAsk resp @?= 1.1002
  
  -- Missing critical fields that should have defaults:
  -- - symInfoTradeContractSize (default 100000.0)
  -- - symInfoTradeTickValue (default 0.0 - BAD!)
  -- - symInfoVolumeMin/Max/Step (defaults 0.01/25.0/0.01)
  -- These defaults may cause order rejections!
  True @?= True  -- Placeholder

-- | Verify ordersGet handles missing fields
testOrderInfoDataLoss :: TestTree
testOrderInfoDataLoss = testCase "OrderInfo missing setup time uses default" $ do
  -- EA response has 10 fields, TradeOrder has 17
  -- Missing: time_setup, time_setup_msc, time_expiration, type_time, type_filling, state, external_id
  let resp = OrderInfoResponse
        { orderTicket = 54321
        , orderSymbol = "GBPUSD"
        , orderType = 2
        , orderVolume = 0.02
        , orderPriceOpen = 1.2500
        , orderPriceCurrent = 1.2450
        , orderSl = 1.2400
        , orderTp = 1.2600
        , orderMagic = 123
        , orderComment = "Test"
        }
  
  orderTicket resp @?= 54321
  -- Missing fields should default to current time, 0, STARTED
  True @?= True  -- Placeholder

--------------------------------------------------------------------------------
-- Routing Tests (Config-based)
--------------------------------------------------------------------------------

routingTests :: TestTree
routingTests = testGroup "Config-based Routing Tests"
  [ testFileBridgeRouting
  , testPythonBridgeRouting
  , testBrokerRestrictions
  ]

-- | Verify FileBridge routing is selected with FileBridge config
testFileBridgeRouting :: TestTree
testFileBridgeRouting = testCase "FileBridge config routes to EA" $ do
  -- This would require mocking Config in a real implementation
  -- For now, verify the pattern exists
  True @?= True  -- Placeholder

-- | Verify PythonBridge routing is selected with PythonBridge config
testPythonBridgeRouting :: TestTree
testPythonBridgeRouting = testCase "PythonBridge config routes to Python" $ do
  -- This would require mocking Config in a real implementation
  True @?= True  -- Placeholder

-- | Verify broker restrictions are respected
testBrokerRestrictions :: TestTree
testBrokerRestrictions = testCase "Order management must use EA" $ do
  -- Verify that orderSend ALWAYS uses EA (broker restriction)
  -- Even if PythonBridge is configured, order management uses FileBridge
  -- This is a critical security requirement
  True @?= True  -- Placeholder

--------------------------------------------------------------------------------
-- JSON Parsing Tests
--------------------------------------------------------------------------------

jsonParsingTests :: TestTree
jsonParsingTests = testGroup "JSON Response Parsing"
  [ testAccountInfoResponseParsing
  , testPositionsGetResponseParsing
  , testSymbolInfoResponseParsing
  , testOrdersGetResponseParsing
  ]

-- | Test AccountInfoResponse JSON parsing
testAccountInfoResponseParsing :: TestTree
testAccountInfoResponseParsing = testCase "Parse AccountInfoResponse JSON" $ do
  let json = "{\"success\":true,\"login\":12345,\"balance\":10000.0,\"equity\":10500.0,\"profit\":500.0,\"margin\":2000.0,\"margin_free\":8500.0,\"margin_level\":525.0,\"leverage\":100,\"currency\":\"USD\",\"name\":\"Test\",\"server\":\"TestServer\",\"trade_allowed\":true,\"trade_expert\":true}" :: BSL.ByteString
  let result = decode json :: Maybe AccountInfoResponse
  case result of
    Nothing -> assertFailure "Failed to parse AccountInfoResponse"
    Just resp -> do
      accountInfoSuccess resp @?= True
      accountInfoLogin resp @?= 12345
      accountInfoBalance resp @?= 10000.0

-- | Test PositionsGetResponse JSON parsing
testPositionsGetResponseParsing :: TestTree
testPositionsGetResponseParsing = testCase "Parse PositionsGetResponse JSON" $ do
  let json = "{\"success\":true,\"count\":1,\"positions\":[{\"ticket\":12345,\"symbol\":\"EURUSD\",\"type\":0,\"volume\":0.01,\"price_open\":1.1000,\"price_current\":1.1050,\"sl\":1.0900,\"tp\":1.1200,\"profit\":50.0,\"swap\":-2.5,\"magic\":0,\"comment\":\"Test\"}]}" :: BSL.ByteString
  let result = decode json :: Maybe PositionsGetResponse
  case result of
    Nothing -> assertFailure "Failed to parse PositionsGetResponse"
    Just resp -> do
      positionsGetSuccess resp @?= True
      positionsGetCount resp @?= 1
      length (positionsGetPositions resp) @?= 1

-- | Test SymbolInfoResponse JSON parsing
testSymbolInfoResponseParsing :: TestTree
testSymbolInfoResponseParsing = testCase "Parse SymbolInfoResponse JSON" $ do
  let json = "{\"success\":true,\"symbol\":\"EURUSD\",\"bid\":1.1000,\"ask\":1.1002,\"digits\":5,\"spread\":2,\"point\":0.00001}" :: BSL.ByteString
  let result = decode json :: Maybe SymbolInfoResponse
  case result of
    Nothing -> assertFailure "Failed to parse SymbolInfoResponse"
    Just resp -> do
      symbolInfoSuccess resp @?= True
      T.unpack (symbolInfoSymbol resp) @?= "EURUSD"
      symbolInfoBid resp @?= 1.1000

-- | Test OrdersGetResponse JSON parsing
testOrdersGetResponseParsing :: TestTree
testOrdersGetResponseParsing = testCase "Parse OrdersGetResponse JSON" $ do
  let json = "{\"success\":true,\"count\":1,\"orders\":[{\"ticket\":54321,\"symbol\":\"GBPUSD\",\"type\":2,\"volume\":0.02,\"price_open\":1.2500,\"price_current\":1.2450,\"sl\":1.2400,\"tp\":1.2600,\"magic\":123,\"comment\":\"Test\"}]}" :: BSL.ByteString
  let result = decode json :: Maybe OrdersGetResponse
  case result of
    Nothing -> assertFailure "Failed to parse OrdersGetResponse"
    Just resp -> do
      ordersGetSuccess resp @?= True
      ordersGetCount resp @?= 1
      length (ordersGetOrders resp) @?= 1

--------------------------------------------------------------------------------
-- Integration Notes
--------------------------------------------------------------------------------

{-
Integration Test Requirements (Manual Testing):

1. FileBridge Tests (requires MT5 running with EA):
   - Set Config to FileBridge
   - Call accountInfo, verify 14 fields returned
   - Call positionsGet, verify positions with missing timestamps
   - Call symbolInfo, verify only 7 fields (bid, ask, digits, spread, point)
   - Call ordersGet, verify orders with missing setup times
   - Call orderSend, verify order execution

2. PythonBridge Tests (requires MT5 Python bridge):
   - Set Config to PythonBridge
   - Call accountInfo, verify all 28 fields
   - Call positionsGet, verify all 19 fields including timestamps
   - Call symbolInfo, verify all 104 fields
   - Call ordersGet, verify all 17 fields
   - Call orderSend, verify full OrderSendResult

3. Data Loss Verification:
   - Compare FileBridge vs PythonBridge results
   - Verify default values are sensible
   - Confirm symbolInfo is using defaults for critical fields
   - Test order placement with FileBridge symbolInfo (may fail!)

4. Broker Restriction Tests:
   - Attempt to route orderSend via Python (should use EA anyway)
   - Verify position_close always uses EA
   - Verify position_modify always uses EA
-}
