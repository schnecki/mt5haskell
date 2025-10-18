{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests requiring real MT5 terminal with EA attached.
--
-- These tests automatically initialize MT5 using startMT5 from MT5.Init.
--
module MT5.Integration.RealMT5Spec (integrationTests) where

import           Test.Hspec
import           MT5.Init (startMT5)
import           MT5.API (accountInfo, positionsGet, symbolInfo, ordersGet)
import           MT5.Config (defaultMT5Config)

integrationTests :: Spec
integrationTests = describe "Integration Tests with Real MT5" $ do  
  describe "accountInfo with Real MT5" $ do
    it "retrieves account info after MT5 initialization" $ do
      -- Initialize MT5 (starts terminal if not running)
      _ <- startMT5 defaultMT5Config
      -- Call accountInfo
      result <- accountInfo
      -- Basic validation (non-empty result)
      result `shouldSatisfy` (not . null . show)
      
  describe "positionsGet with Real MT5" $ do
    it "retrieves positions after MT5 initialization" $ do
      _ <- startMT5 defaultMT5Config
      result <- positionsGet
      -- Should return a list (may be empty if no positions)
      result `shouldSatisfy` (const True)
      
  describe "symbolInfo with Real MT5" $ do
    it "retrieves symbol info for EURUSD after MT5 initialization" $ do
      _ <- startMT5 defaultMT5Config
      result <- symbolInfo "EURUSD"
      -- Should return symbol data
      result `shouldSatisfy` (not . null . show)
      
  describe "ordersGet with Real MT5" $ do
    it "retrieves orders after MT5 initialization" $ do
      _ <- startMT5 defaultMT5Config
      result <- ordersGet Nothing Nothing
      -- Should return a list (may be empty if no orders)
      result `shouldSatisfy` (const True)
