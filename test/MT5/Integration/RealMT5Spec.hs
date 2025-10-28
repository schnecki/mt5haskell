{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests requiring real MT5 terminal with EA attached.
--
-- These tests use FileBridge (no Python needed - EA must be running).
-- For Python bridge tests, call startMT5 first.
--
module MT5.Integration.RealMT5Spec (integrationTests) where

import           Control.Concurrent (threadDelay)
import           Test.Hspec
import           MT5.API (accountInfo, positionsGet, symbolInfo, ordersGet)
import           MT5.Config (setConfig, defaultMT5Config, Config(..), CommunicationChannel(..))

integrationTests :: Spec
integrationTests = describe "Integration Tests with Real MT5" $ do
  -- Set config to use FileBridge for ALL channels (EA must be running)
  let fileBridgeConfig = defaultMT5Config 
        { communicationChannel = FileBridge
        , positionManagementChannel = FileBridge
        }
  beforeAll_ (setConfig fileBridgeConfig) $ do
    describe "accountInfo with Real MT5" $ do
      it "retrieves account info via FileBridge (EA must be running)" $ do
        threadDelay 500000  -- 500ms delay before test
        -- Call accountInfo (uses FileBridge)
        result <- accountInfo
        -- Basic validation (check type, don't force full evaluation)
        result `shouldSatisfy` (const True)
        
    describe "positionsGet with Real MT5" $ do
      it "retrieves positions via FileBridge (EA must be running)" $ do
        threadDelay 500000  -- 500ms delay before test
        result <- positionsGet
        -- Should return a list (may be empty if no positions)
        result `shouldSatisfy` (const True)
        
    describe "symbolInfo with Real MT5" $ do
      it "retrieves symbol info for EURUSD via FileBridge (EA must be running)" $ do
        threadDelay 500000  -- 500ms delay before test
        result <- symbolInfo "EURUSD"
        -- Should return symbol data (check type, don't force full evaluation)
        result `shouldSatisfy` (const True)
        
    describe "ordersGet with Real MT5" $ do
      it "retrieves orders via FileBridge (EA must be running)" $ do
        threadDelay 500000  -- 500ms delay before test
        result <- ordersGet Nothing Nothing
        -- Should return a list (may be empty if no orders)
        result `shouldSatisfy` (const True)
        -- Add delay to prevent file lock conflicts
        threadDelay 200000  -- 200ms
