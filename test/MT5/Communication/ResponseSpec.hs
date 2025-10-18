{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MT5.Communication.ResponseSpec (spec) where

import           Data.Aeson (eitherDecode, ToJSON, toEncoding, encode)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.ByteString.Lazy as BL
import           Test.Hspec

import           MT5.Communication.Response


-- Helper to encode aeson Values to ByteString
encodeValue :: ToJSON a => a -> BL.ByteString
encodeValue = encode


spec :: Spec
spec = do
  describe "OrderSendResponse" $ do
    it "parses successful order response" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "retcode": 10009,
          "deal": 123456,
          "order": 789012,
          "volume": 0.01,
          "price": 1.1000,
          "comment": "Order placed"
        }
      |]
      let result = eitherDecode (encode json) :: Either String OrderSendResponse
      result `shouldSatisfy` isRight
      case result of
        Right resp -> do
          orderSendRespSuccess resp `shouldBe` True
          orderSendRespRetcode resp `shouldBe` 10009
          orderSendRespDeal resp `shouldBe` 123456
        Left _ -> expectationFailure "Failed to parse"

    it "handles missing comment field" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "retcode": 10009,
          "deal": 123456,
          "order": 789012,
          "volume": 0.01,
          "price": 1.1000
        }
      |]
      let result = eitherDecode (encode json) :: Either String OrderSendResponse
      result `shouldSatisfy` isRight

  describe "PositionCloseResponse" $ do
    it "parses successful position close" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "retcode": 10009,
          "deal": 123456,
          "order": 789012,
          "volume": 0.01
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String PositionCloseResponse
      result `shouldSatisfy` isRight
      case result of
        Right resp -> do
          positionCloseSuccess resp `shouldBe` True
          positionCloseRetcode resp `shouldBe` 10009
          positionCloseDeal resp `shouldBe` 123456
        Left _ -> expectationFailure "Failed to parse"

    it "handles optional fields" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "retcode": 10009,
          "deal": 123456
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String PositionCloseResponse
      result `shouldSatisfy` isRight

  describe "PositionModifyResponse" $ do
    it "parses successful position modify" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "retcode": 10009,
          "sl": 1.0900,
          "tp": 1.1100
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String PositionModifyResponse
      result `shouldSatisfy` isRight

  describe "SymbolInfoResponse" $ do
    it "parses symbol info correctly" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "symbol": "EURUSD",
          "bid": 1.15910,
          "ask": 1.17110,
          "digits": 5,
          "spread": 1200,
          "point": 0.00001
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String SymbolInfoResponse
      result `shouldSatisfy` isRight
      case result of
        Right resp -> do
          symbolInfoSuccess resp `shouldBe` True
          symbolInfoSymbol resp `shouldBe` "EURUSD"
          symbolInfoBid resp `shouldBe` 1.15910
          symbolInfoAsk resp `shouldBe` 1.17110
          symbolInfoDigits resp `shouldBe` 5
          symbolInfoSpread resp `shouldBe` 1200
        Left _ -> expectationFailure "Failed to parse"

  describe "SymbolSelectResponse" $ do
    it "parses symbol select correctly" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "symbol": "GBPUSD",
          "selected": true
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String SymbolSelectResponse
      result `shouldSatisfy` isRight

  describe "SymbolsGetResponse" $ do
    it "parses symbols list correctly" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "count": 3,
          "symbols": ["EURUSD", "GBPUSD", "USDJPY"]
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String SymbolsGetResponse
      result `shouldSatisfy` isRight
      case result of
        Right resp -> do
          symbolsGetSuccess resp `shouldBe` True
          symbolsGetCount resp `shouldBe` 3
          length (symbolsGetSymbols resp) `shouldBe` 3
        Left _ -> expectationFailure "Failed to parse"

  describe "OHLCVCandle" $ do
    it "parses candle data correctly" $ do
      let json = [aesonQQ|
        {
          "time": 1634567890,
          "open": 1.1000,
          "high": 1.1050,
          "low": 1.0950,
          "close": 1.1020,
          "volume": 1500,
          "spread": 3,
          "real_volume": 250.5
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String OHLCVCandle
      result `shouldSatisfy` isRight
      case result of
        Right candle -> do
          candleTime candle `shouldBe` 1634567890
          candleOpen candle `shouldBe` 1.1000
          candleHigh candle `shouldBe` 1.1050
          candleLow candle `shouldBe` 1.0950
          candleClose candle `shouldBe` 1.1020
          candleVolume candle `shouldBe` 1500
          candleSpread candle `shouldBe` 3
          candleRealVolume candle `shouldBe` 250.5
        Left _ -> expectationFailure "Failed to parse"

  describe "CandlesGetResponse" $ do
    it "parses candles response correctly" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "symbol": "EURUSD",
          "timeframe": "H1",
          "count": 2,
          "candles": [
            {
              "time": 1634567890,
              "open": 1.1000,
              "high": 1.1050,
              "low": 1.0950,
              "close": 1.1020,
              "volume": 1500,
              "spread": 3,
              "real_volume": 250.5
            },
            {
              "time": 1634571490,
              "open": 1.1020,
              "high": 1.1080,
              "low": 1.1010,
              "close": 1.1075,
              "volume": 1800,
              "spread": 4,
              "real_volume": 300.2
            }
          ]
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String CandlesGetResponse
      result `shouldSatisfy` isRight
      case result of
        Right resp -> do
          candlesGetSuccess resp `shouldBe` True
          candlesGetSymbol resp `shouldBe` "EURUSD"
          candlesGetTimeframe resp `shouldBe` "H1"
          candlesGetCount resp `shouldBe` 2
          length (candlesGetCandles resp) `shouldBe` 2
        Left _ -> expectationFailure "Failed to parse"

  describe "AccountInfoResponse" $ do
    it "parses account info correctly" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "login": 12345678,
          "balance": 10000.50,
          "equity": 10050.25,
          "profit": 50.25,
          "margin": 500.00,
          "margin_free": 9550.25,
          "margin_level": 2010.05,
          "leverage": 100,
          "currency": "USD",
          "name": "Test Account",
          "server": "Demo-Server",
          "trade_allowed": true,
          "trade_expert": true
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String AccountInfoResponse
      result `shouldSatisfy` isRight
      case result of
        Right resp -> do
          accountInfoSuccess resp `shouldBe` True
          accountInfoLogin resp `shouldBe` 12345678
          accountInfoBalance resp `shouldBe` 10000.50
          accountInfoEquity resp `shouldBe` 10050.25
          accountInfoLeverage resp `shouldBe` 100
          accountInfoCurrency resp `shouldBe` "USD"
        Left _ -> expectationFailure "Failed to parse"

  describe "PositionInfoResponse" $ do
    it "parses position info correctly" $ do
      let json = [aesonQQ|
        {
          "ticket": 123456,
          "symbol": "EURUSD",
          "type": 0,
          "volume": 0.01,
          "price_open": 1.1000,
          "price_current": 1.1020,
          "sl": 1.0900,
          "tp": 1.1100,
          "profit": 2.50,
          "swap": -0.10,
          "magic": 0,
          "comment": "Test position"
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String PositionInfoResponse
      result `shouldSatisfy` isRight

  describe "PositionsGetResponse" $ do
    it "parses positions list correctly" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "count": 1,
          "positions": [
            {
              "ticket": 123456,
              "symbol": "EURUSD",
              "type": 0,
              "volume": 0.01,
              "price_open": 1.1000,
              "price_current": 1.1020,
              "sl": 1.0900,
              "tp": 1.1100,
              "profit": 2.50,
              "swap": -0.10,
              "magic": 0,
              "comment": "Test"
            }
          ]
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String PositionsGetResponse
      result `shouldSatisfy` isRight

    it "parses empty positions list" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "count": 0,
          "positions": []
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String PositionsGetResponse
      result `shouldSatisfy` isRight
      case result of
        Right resp -> do
          positionsGetCount resp `shouldBe` 0
          positionsGetPositions resp `shouldBe` []
        Left _ -> expectationFailure "Failed to parse"

  describe "OrderInfoResponse" $ do
    it "parses order info correctly" $ do
      let json = [aesonQQ|
        {
          "ticket": 789012,
          "symbol": "GBPUSD",
          "type": 2,
          "volume": 0.02,
          "price_open": 1.3000,
          "price_current": 1.2995,
          "sl": 1.2900,
          "tp": 1.3100,
          "magic": 12345,
          "comment": "Pending order"
        }
      |]
      let result = eitherDecode (encodeValue json) :: Either String OrderInfoResponse
      result `shouldSatisfy` isRight

  describe "OrdersGetResponse" $ do
    it "parses orders list correctly" $ do
      let json = [aesonQQ|
        {
          "success": true,
          "count": 1,
          "orders": [
            {
              "ticket": 789012,
              "symbol": "GBPUSD",
              "type": 2,
              "volume": 0.02,
              "price_open": 1.3000,
              "price_current": 1.2995,
              "sl": 1.2900,
              "tp": 1.3100,
              "magic": 12345,
              "comment": "Test"
            }
          ]
        }
      |]
      let result = eitherDecode (encode json) :: Either String OrdersGetResponse
      result `shouldSatisfy` isRight


-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
