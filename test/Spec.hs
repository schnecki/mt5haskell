module Main (main) where

import Test.Tasty

-- Import test modules
import MT5.APISpec (apiTests)
import MT5.Data.CurrentPriceSpec (currentPriceTests)
import MT5.Data.CandleSpec (candleTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MT5 Tests"
  [ apiTests  
  , currentPriceTests
  , candleTests
  ]
