module Main (main) where

import Test.Tasty

-- Import test modules
import MT5.APISpec (apiTests)
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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MT5 Tests"
  [ apiTests  
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
  ]
