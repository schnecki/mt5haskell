module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec

-- Import test modules
import MT5.APISpec (apiTests)
import MT5.API.DualChannelSpec (dualChannelTests)
import MT5.Data.CurrentPriceSpec (currentPriceTests)
import MT5.Data.CandleSpec (candleTests)
import qualified MT5.Data.AccountInfoSpec as AccountInfoSpec
import qualified MT5.Data.SymbolInfoSpec as SymbolInfoSpec
import qualified MT5.Data.TradeOrderSpec as TradeOrderSpec
-- Phase 2: Error Handling & Edge Cases
import qualified MT5.UtilSpec as UtilSpec
import qualified MT5.CommunicationSpec as CommunicationSpec
import qualified MT5.ConfigSpec as ConfigSpec
-- Phase 3: Integration & Full API Testing
import qualified MT5.InitSpec as InitSpec
import qualified MT5.LoggingSpec as LoggingSpec
import qualified MT5.PyProcSpec as PyProcSpec
-- Phase 4: Advanced Business Logic & Trading Calculations
import qualified MT5.BusinessLogicSpec as BusinessLogicSpec
-- Phase 6: Integration & Error Recovery Testing
import qualified MT5.Integration.RealMT5Spec as RealMT5Spec
import qualified MT5.Integration.ErrorRecoverySpec as ErrorRecoverySpec
import qualified MT5.Integration.FileCorruptionSpec as FileCorruptionSpec

main :: IO ()
main = do
  -- Convert Hspec specs to Tasty TestTree
  integrationTests <- testSpec "Integration Tests with Real MT5" RealMT5Spec.integrationTests
  errorRecoveryTests <- testSpec "Error Recovery Tests" ErrorRecoverySpec.errorRecoveryTests
  fileCorruptionTests <- testSpec "File Corruption Handling Tests" FileCorruptionSpec.fileCorruptionTests
  defaultMain (tests integrationTests errorRecoveryTests fileCorruptionTests)

tests :: TestTree -> TestTree -> TestTree -> TestTree
tests integrationTests errorRecoveryTests fileCorruptionTests = testGroup "MT5 Tests"
  [ apiTests
  , dualChannelTests  -- Phase 4: Dual channel routing tests
  , currentPriceTests
  , candleTests
  -- Phase 1: Pure Data Types & Utilities
  , AccountInfoSpec.spec
  , SymbolInfoSpec.spec
  , TradeOrderSpec.spec
  -- Phase 2: Error Handling & Edge Cases
  , UtilSpec.spec
  , CommunicationSpec.spec
  , ConfigSpec.spec
  -- Phase 3: Integration & Full API Testing
  , InitSpec.spec
  , LoggingSpec.spec
  , PyProcSpec.spec
  -- Phase 4: Advanced Business Logic & Trading Calculations
  , BusinessLogicSpec.spec
  -- Phase 6: Integration & Error Recovery Testing
  , integrationTests
  , errorRecoveryTests
  , fileCorruptionTests
  ]
