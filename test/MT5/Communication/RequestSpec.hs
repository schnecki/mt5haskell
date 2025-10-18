{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MT5.Communication.RequestSpec (spec) where

import           Data.Aeson (encode, decode, toJSON)
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as BL
import           Data.Either (isLeft, isRight)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck.Instances.Time ()

import           MT5.Communication.Request
import           MT5.Data.OrderType
import           MT5.Data.OrderTypeFilling
import           MT5.Data.OrderTypeTime


spec :: Spec
spec = do
  describe "OrderSendRequest" $ do
    it "creates valid request with positive volume" $ do
      let result = mkOrderSendRequest "EURUSD" 0.01 ORDER_TYPE_BUY 1.1000 0.0 0.0 10 
                                      ORDER_FILLING_FOK ORDER_TIME_GTC "test"
      result `shouldSatisfy` isRight

    it "rejects empty symbol" $ do
      let result = mkOrderSendRequest "" 0.01 ORDER_TYPE_BUY 1.1000 0.0 0.0 10 
                                      ORDER_FILLING_FOK ORDER_TIME_GTC "test"
      result `shouldBe` Left (InvalidSymbol "")

    it "rejects negative volume" $ do
      let result = mkOrderSendRequest "EURUSD" (-0.01) ORDER_TYPE_BUY 1.1000 0.0 0.0 10 
                                      ORDER_FILLING_FOK ORDER_TIME_GTC "test"
      result `shouldBe` Left (InvalidVolume (-0.01))

    it "rejects zero volume" $ do
      let result = mkOrderSendRequest "EURUSD" 0.0 ORDER_TYPE_BUY 1.1000 0.0 0.0 10 
                                      ORDER_FILLING_FOK ORDER_TIME_GTC "test"
      result `shouldBe` Left (InvalidVolume 0.0)

    it "rejects negative price" $ do
      let result = mkOrderSendRequest "EURUSD" 0.01 ORDER_TYPE_BUY (-1.1000) 0.0 0.0 10 
                                      ORDER_FILLING_FOK ORDER_TIME_GTC "test"
      result `shouldBe` Left (InvalidPrice (-1.1000))

    it "serializes to correct JSON format" $ do
      let Right req = mkOrderSendRequest "EURUSD" 0.01 ORDER_TYPE_BUY 1.1000 1.0900 1.1100 10 
                                         ORDER_FILLING_FOK ORDER_TIME_GTC "test order"
      let json = toJSON req
      let expected = [aesonQQ|
        {
          "symbol": "EURUSD",
          "volume": 0.01,
          "type": 0,
          "price": 1.1000,
          "sl": 1.0900,
          "tp": 1.1100,
          "deviation": 10,
          "type_filling": 2,
          "type_time": 0,
          "comment": "test order"
        }
      |]
      json `shouldBe` expected

  describe "PositionCloseRequest" $ do
    it "creates valid request with positive ticket" $ do
      let result = mkPositionCloseRequest 123456
      result `shouldSatisfy` isRight

    it "rejects zero ticket" $ do
      let result = mkPositionCloseRequest 0
      result `shouldBe` Left (InvalidTicket 0)

    it "rejects negative ticket" $ do
      let result = mkPositionCloseRequest (-123)
      result `shouldBe` Left (InvalidTicket (-123))

    it "serializes to correct JSON format" $ do
      let Right req = mkPositionCloseRequest 123456
      let json = toJSON req
      let expected = [aesonQQ|{"ticket": 123456}|]
      json `shouldBe` expected

  describe "PositionClosePartialRequest" $ do
    it "creates valid request" $ do
      let result = mkPositionClosePartialRequest 123456 0.5
      result `shouldSatisfy` isRight

    it "rejects invalid ticket" $ do
      let result = mkPositionClosePartialRequest 0 0.5
      result `shouldBe` Left (InvalidTicket 0)

    it "rejects zero volume" $ do
      let result = mkPositionClosePartialRequest 123456 0.0
      result `shouldBe` Left (InvalidVolume 0.0)

    it "rejects negative volume" $ do
      let result = mkPositionClosePartialRequest 123456 (-0.5)
      result `shouldBe` Left (InvalidVolume (-0.5))

  describe "PositionModifyRequest" $ do
    it "creates valid request" $ do
      let result = mkPositionModifyRequest 123456 1.0900 1.1100
      result `shouldSatisfy` isRight

    it "allows zero SL/TP (means remove)" $ do
      let result = mkPositionModifyRequest 123456 0.0 0.0
      result `shouldSatisfy` isRight

    it "rejects invalid ticket" $ do
      let result = mkPositionModifyRequest (-1) 1.0900 1.1100
      result `shouldBe` Left (InvalidTicket (-1))

    it "rejects negative SL" $ do
      let result = mkPositionModifyRequest 123456 (-1.0900) 1.1100
      result `shouldBe` Left (InvalidPrice (-1.0900))

    it "rejects negative TP" $ do
      let result = mkPositionModifyRequest 123456 1.0900 (-1.1100)
      result `shouldBe` Left (InvalidPrice (-1.1100))

  describe "SymbolInfoRequest" $ do
    it "creates valid request" $ do
      let result = mkSymbolInfoRequest "EURUSD"
      result `shouldSatisfy` isRight

    it "rejects empty symbol" $ do
      let result = mkSymbolInfoRequest ""
      result `shouldBe` Left (InvalidSymbol "")

    it "serializes to correct JSON format" $ do
      let Right req = mkSymbolInfoRequest "EURUSD"
      let json = toJSON req
      let expected = [aesonQQ|{"symbol": "EURUSD"}|]
      json `shouldBe` expected

  describe "SymbolSelectRequest" $ do
    it "creates valid request to enable symbol" $ do
      let result = mkSymbolSelectRequest "GBPUSD" True
      result `shouldSatisfy` isRight

    it "creates valid request to disable symbol" $ do
      let result = mkSymbolSelectRequest "GBPUSD" False
      result `shouldSatisfy` isRight

    it "rejects empty symbol" $ do
      let result = mkSymbolSelectRequest "" True
      result `shouldBe` Left (InvalidSymbol "")

  describe "SymbolsGetRequest" $ do
    it "creates request with no filter" $ do
      let req = mkSymbolsGetRequest Nothing
      toJSON req `shouldBe` [aesonQQ|{"group": null}|]

    it "creates request with group filter" $ do
      let req = mkSymbolsGetRequest (Just "Forex")
      toJSON req `shouldBe` [aesonQQ|{"group": "Forex"}|]

  describe "CandlesGetRequest" $ do
    it "creates valid range request" $ do
      let result = mkCandlesGetRequest "EURUSD" "H1" CandleModeRange 
                                       (Just undefined) (Just undefined) Nothing 100
      -- Note: Would need real UTCTime values for proper test
      result `shouldSatisfy` isRight

    it "creates valid from request" $ do
      let result = mkCandlesGetRequest "EURUSD" "H1" CandleModeFrom 
                                       (Just undefined) Nothing Nothing 100
      result `shouldSatisfy` isRight

    it "creates valid from_pos request" $ do
      let result = mkCandlesGetRequest "EURUSD" "H1" CandleModeFromPos 
                                       Nothing Nothing (Just 0) 100
      result `shouldSatisfy` isRight

    it "rejects empty symbol" $ do
      let result = mkCandlesGetRequest "" "H1" CandleModeFrom 
                                       (Just undefined) Nothing Nothing 100
      result `shouldBe` Left (InvalidSymbol "")

    it "rejects empty timeframe" $ do
      let result = mkCandlesGetRequest "EURUSD" "" CandleModeFrom 
                                       (Just undefined) Nothing Nothing 100
      result `shouldSatisfy` isLeft

    it "rejects zero count" $ do
      let result = mkCandlesGetRequest "EURUSD" "H1" CandleModeFrom 
                                       (Just undefined) Nothing Nothing 0
      result `shouldBe` Left (InvalidCount 0)

    it "rejects negative count" $ do
      let result = mkCandlesGetRequest "EURUSD" "H1" CandleModeFrom 
                                       (Just undefined) Nothing Nothing (-10)
      result `shouldBe` Left (InvalidCount (-10))

  describe "AccountInfoRequest" $ do
    it "creates valid request" $ do
      let req = mkAccountInfoRequest
      toJSON req `shouldBe` [aesonQQ|{}|]

  describe "PositionsGetRequest" $ do
    it "creates request with no filter" $ do
      let req = mkPositionsGetRequest Nothing
      toJSON req `shouldBe` [aesonQQ|{"symbol": null}|]

    it "creates request with symbol filter" $ do
      let req = mkPositionsGetRequest (Just "EURUSD")
      toJSON req `shouldBe` [aesonQQ|{"symbol": "EURUSD"}|]

  describe "OrdersGetRequest" $ do
    it "creates request with no filter" $ do
      let req = mkOrdersGetRequest Nothing
      toJSON req `shouldBe` [aesonQQ|{"symbol": null}|]

    it "creates request with symbol filter" $ do
      let req = mkOrdersGetRequest (Just "EURUSD")
      toJSON req `shouldBe` [aesonQQ|{"symbol": "EURUSD"}|]
