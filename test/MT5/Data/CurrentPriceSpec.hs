module MT5.Data.CurrentPriceSpec (currentPriceTests) where

import Data.Bits ((.&.), (.|.))
import qualified Data.Text as T
import Data.Time (parseTimeOrError)
import Data.Time.Format (defaultTimeLocale)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

import MT5.Data.CurrentPrice

currentPriceTests :: TestTree
currentPriceTests = testGroup "CurrentPrice Tests"
  [ currentPriceUnitTests
  , currentPricePropertyTests
  ]

currentPriceUnitTests :: TestTree
currentPriceUnitTests = testGroup "CurrentPrice Unit Tests"
  [ testSpreadCalculation
  , testValidPrice
  , testTickFlags
  ]

testSpreadCalculation :: TestTree
testSpreadCalculation = testCase "Spread calculation" $ do
  -- Use assertBool with tolerance for floating point comparisons
  let eps = 1e-10
      assertApproxEqual expected actual = 
        assertBool ("Expected " ++ show expected ++ " but got " ++ show actual) 
                   (abs (expected - actual) < eps)
  
  assertApproxEqual 0.0002 (calculateSpread 1.0852 1.0850)
  assertApproxEqual 0.5 (calculateSpread 100.0 99.5)
  assertApproxEqual 0.0 (calculateSpread 1.0 1.0)

testValidPrice :: TestTree
testValidPrice = testCase "Valid price checking" $ do
  let epochTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "1970-01-01 00:00:00 UTC"
      validPrice = CurrentPrice (T.pack "EURUSD") 1.0850 1.0852 0.0002 0.0 0 epochTime 0 0 0.0
      invalidPrice1 = CurrentPrice (T.pack "INVALID") 0.0 1.0852 0.0002 0.0 0 epochTime 0 0 0.0
      invalidPrice2 = CurrentPrice (T.pack "INVALID") 1.0850 0.0 0.0002 0.0 0 epochTime 0 0 0.0
  
  assertBool "Valid price should be valid" (isValidPrice validPrice)
  assertBool "Zero bid should be invalid" (not $ isValidPrice invalidPrice1)
  assertBool "Zero ask should be invalid" (not $ isValidPrice invalidPrice2)

testTickFlags :: TestTree
testTickFlags = testCase "Tick flag operations" $ do
  let epochTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "1970-01-01 00:00:00 UTC"
      cp = CurrentPrice (T.pack "TEST") 1.0 1.0 0.0 0.0 0 epochTime 0 (tickFlagBid .|. tickFlagAsk) 0.0
  
  assertBool "Should have bid flag" (hasTickFlag cp tickFlagBid)
  assertBool "Should have ask flag" (hasTickFlag cp tickFlagAsk)
  assertBool "Should not have last flag" (not $ hasTickFlag cp tickFlagLast)
  
  -- Test individual flags
  tickFlagBid @?= 2
  tickFlagAsk @?= 4
  tickFlagLast @?= 8
  tickFlagVolume @?= 16
  tickFlagBuy @?= 32
  tickFlagSell @?= 64

currentPricePropertyTests :: TestTree  
currentPricePropertyTests = testGroup "CurrentPrice Property Tests"
  [ testProperty "Spread calculation property" prop_spreadCalculation
  , testProperty "Valid price property" prop_validPricePositive
  , testProperty "Tick flag consistency" prop_tickFlagConsistency
  ]

-- Property-based tests for CurrentPrice functions
prop_spreadCalculation :: NonNegative Double -> NonNegative Double -> Property  
prop_spreadCalculation (NonNegative spread) (NonNegative bid) = 
  let ask = bid + spread
      result = calculateSpread ask bid
      eps = 1e-10
  in counterexample ("Expected " ++ show spread ++ " but got " ++ show result) $
     abs (result - spread) < eps

prop_validPricePositive :: Positive Double -> Positive Double -> Bool
prop_validPricePositive (Positive bid) (Positive ask) = 
  let epochTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "1970-01-01 00:00:00 UTC"
      cp = CurrentPrice (T.pack "TEST") bid ask 0 0 0 epochTime 0 0 0
  in isValidPrice cp

prop_tickFlagConsistency :: Property
prop_tickFlagConsistency = forAll (elements [tickFlagBid, tickFlagAsk, tickFlagLast, tickFlagVolume, tickFlagBuy, tickFlagSell]) $ \flag ->
  forAll arbitrary $ \flags ->
  let epochTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "1970-01-01 00:00:00 UTC"
      cp = CurrentPrice (T.pack "TEST") 0 0 0 0 0 epochTime 0 flags 0
  in hasTickFlag cp flag === ((flags .&. flag) /= 0)
