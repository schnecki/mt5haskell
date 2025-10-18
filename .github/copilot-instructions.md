# Enhanced Copilot Instructions for Haskell Trading System

This document provides comprehensive guidelines for GitHub Copilot when working with Haskell trading systems. It emphasizes functional programming principles, type safety, and cognitive load reduction while maintaining production-ready code quality.

---

## 🚨 CRITICAL WORKFLOW REQUIREMENTS

### 🔄 Mandatory Feedback Loop (NEVER SKIP)
After completing ANY task and always before prompting, execute this EXACT sequence:

```bash
# 1. Open feedback file (in project root folder)
notify-send "Copilot is ready for feedback!" && emacs -q FEEDBACK.md &

# 2. Display keep files message (DO NOT PRINT ANYTHING ELSE IN THIS COMMAND)
printf '\n\nKeep files if you wish\n\n'

# 3. Read and integrate ALL feedback from FEEDBACK.md. Do not prompt until feedback is empty.
```

**Continue iterating until**: User provides empty feedback OR explicitly confirms completion.

!!!CRITICAL, NEVER FORGET!!!: YOU HAVE TO FIRST ASK FOR FEEDBACK BEFORE PROMPTING AND THEN IMPLEMENT THE FEEDBACK ENTIRELY! Before you end your tasks and before you prompt, print
to the command line the current status (including a task summary and a list of changed files) and ask for
feedback on the current tasks. To ask for feedback you must first run `emacs -q FEEDBACK.md`, second
and before reading the FEEDBACK.md file, execute `printf '\n\nKeep files if you wish\n\n'`, and
finally (iii) read the FEEDBACK.md file. Read the file contents and take the feedback seriously.
Update the todo list with the tasks from the feedback. Keep iterating until you have integrated all feedback points
raised by the human user. Only prompt if the user leaves the feedback empty or agrees that you have accomplished all tasks.

CRITICAL: DO NOT write to the FEEDBACK.md file! Do not clear the FEEDBACK.md file! Do not edit the FEEDBACK.md file! Do not touch the FEEDBACK.md file in any way! You are ONLY allowed to READ it! NEVER use replace_string_in_file, create_file, or any editing tool on FEEDBACK.md!

### 📋 Task Documentation Protocol

#### Required Files (ALWAYS MAINTAIN)
- **`plans/TASK_SUMMARY_OVERVIEW.md`**: Current epic with features (working phases) and completion status. Be coincise. Create lists with checkmarks.
- **`plans/TASK_SUMMARY_DETAILED_PHASE_PLAN.md`**: Current feature to work on and PBIs/stages/tasks and detailed implementation plan

IMPORTANT: Any document you create must go into the folder plans/, located on the root directory.

**Rules**:
1. **Read entire file** before ANY modification
2. **Update only status/completion markers** (preserve all task content)
3. **Use ✅ for completed** tasks with detailed success descriptions
4. **Update documentation BEFORE** requesting feedback

**Additional plans**:
1. Ensure to write holistic plans. Consider adding the following sections (but don't overload the file! Be rigorous
  but concise!): Executive Summary, Phases and Tasks, Current System
  Analysis with Architecture, Process, Implementation Plan (if
  applicable), Models Created and Used, Evaluation Parameters, (Bash)
  Commands used for Result Generation, Testing and Validation Strategy (if
  applicable), Results, Quality Assurance Metrics and Code Quality Assessment (if
  applicable), Risk Assessment and Mitigation, Success Metrics and KPIs, Critical Deliverables (a list
  of deliverables to checkmark), Limitations and Critical Issues, Recommendation (Immediate,
  Medium-Term, Long-Term), Improvement Ideas (if any), Items for Complete Implementation (if
  applicable), Implementation Timeline, Conclusion.
2. Iterate at least once over the
  plans, but ensure that you are satisfied. Ensure to always update these files according to the
  implementation status before asking for feedback!

---
# Update Protocol (CRITICAL)

```bash
# ✅ CORRECT: Always read full file first
read_file $FILE 1 END
# Then update
```

This ensures that any changes made by the user are preserved and not lost.

---

## 🏗️ PROJECT-SPECIFIC REQUIREMENTS

### 🔧 Build System (Critical Rules)
```yaml
# ✅ PRIMARY: package.yaml (edit this)
name: aral-trader
dependencies:
  - base
  - vector

# ❌ NEVER EDIT: .cabal files (auto-generated)
```

**Stack Workflow**:
```bash
# Build process
stack build --fast

# For testing only run the current package tests, but NEVER dependencies (do not use `stack test`!)
```

### 📁 Project Organization (Mandatory)
**File Organization Rules**:
- **`scripts/`**: ALL test files, debug scripts, benchmarks, and utility scripts
  - Test shell scripts (*.sh)
  - Debug logs (*.log)
  - Python test files (*test*.py)
  - Stress tests and performance benchmarks
  - Backup/temporary files
- **`plans/`**: Documentation and implementation plans
- **`examples/`**: Working code examples and main programs
- **`src/`**: Core library modules only

**CRITICAL**: Never leave test files, debug scripts, or temporary files in root directory. Always move to `scripts/` folder.

### 🧪 Testing Standards (Non-Negotiable)

#### What You MUST Do
```haskell
-- ✅ Real implementations with actual market data
testStrategy :: Spec
testStrategy = it "works with real data" $ do
  marketData <- loadRealData "EURUSD" (days 30)  -- Real data!
  result <- runStrategy config marketData
  result `shouldSatisfy` isValid

-- ✅ Use actual modules from src/
import Trader.Strategy.BollingerBands  -- Real module
import Trader.Data.Kraken             -- Real data source
```

#### What You MUST NOT Do
```haskell
-- ❌ FORBIDDEN: Mock market data
mockData = [1.0, 1.1, 1.2]  -- Don't do this!

-- ❌ FORBIDDEN: Placeholder functions
testPlaceholder = it "works" $ pending  -- Don't do this!

-- ❌ FORBIDDEN: Skip failing tests
-- describe "broken test" $ do  -- Don't comment out!
```

#### Model Files Requirement
```bash
# Generate real binary models BEFORE testing
./generate-model BollingerBands
./generate-model RSIMeanReversion

# Tests MUST load real model files
models/BollingerBands.bin  # Must exist
models/RSIMeanReversion.bin  # Must exist
```

### 📝 Code Quality Standards (Mandatory)

#### Type Signatures (Required for ALL functions)
```haskell
-- ✅ REQUIRED: Explicit type signatures
calculateRSI :: Int -> [Double] -> Maybe Double
calculateRSI period prices = ...

-- ❌ FORBIDDEN: Missing type signatures
calculateRSI period prices = ...  -- Compiler will complain
```

#### Error Handling (No Exceptions for Logic)
```haskell
-- ✅ GOOD: Explicit error handling
safeDivide :: Double -> Double -> Either Text Double
safeDivide _ 0 = Left "Division by zero"
safeDivide x y = Right (x / y)

-- ❌ AVOID: Partial functions
unsafeDivide x y = x / y  -- Can crash on division by zero!
```

#### Documentation (Haddock Required)
```haskell
-- | Calculate Simple Moving Average over specified period.
--
-- Returns 'Nothing' if insufficient data points available.
--
-- ==== __Examples__
--
-- >>> calculateSMA 3 [1.0, 2.0, 3.0, 4.0]
-- Just 3.0
--
-- @since 0.1.0.0
calculateSMA :: Int -> [Double] -> Maybe Double
```

---

## 📚 CORE HASKELL PRINCIPLES

### 1. Purity and Referential Transparency
**Maximize pure functions** - same input always produces same output with no side effects.

```haskell
-- ✅ Good: Pure function
calculateMovingAverage :: [Double] -> Int -> Double
calculateMovingAverage prices period =
  let recent = take period prices
  in sum recent / fromIntegral (length recent)

-- ❌ Avoid: Side effects in core logic
calculateMovingAverageIO :: [Double] -> Int -> IO Double
```

**Benefits**: Easier testing, reasoning, and composition. Eliminates state-related bugs.

### 2. Type-Driven Development
**Make illegal states unrepresentable** through precise type design.

```haskell
-- ✅ Good: Prevents invalid periods
newtype Period = Period { unPeriod :: Int }
mkPeriod :: Int -> Maybe Period
mkPeriod n | n > 0 = Just (Period n)
           | otherwise = Nothing

-- ❌ Avoid: Allows invalid values
type Period = Int  -- Could be negative!
```

**Benefits**: Compile-time guarantees, self-documenting code, fewer runtime errors.

### 3. Algebraic Data Types (ADTs)
**Model domain precisely** with sum and product types.

```haskell
-- ✅ Good: Explicit trading signal types
data TradingSignal
  = Buy { strength :: SignalStrength, reason :: Text }
  | Sell { strength :: SignalStrength, reason :: Text }
  | Hold { reason :: Text }
  deriving (Show, Eq)

-- ✅ Good: Constrained signal strength
newtype SignalStrength = SignalStrength Double
mkSignalStrength :: Double -> Maybe SignalStrength
mkSignalStrength s | s >= 0.1 && s <= 1.0 = Just (SignalStrength s)
                   | otherwise = Nothing
```

**Benefits**: Exhaustive pattern matching, impossible invalid states, clear domain modeling.

---

## 🏛️ ARCHITECTURAL PATTERNS

### 🎯 Functional Core, Imperative Shell (Primary Pattern)

**Philosophy**: Keep business logic pure, push side effects to edges.

```
┌─────────────────────────────────────┐
│       Main / CLI (app/Main.hs)     │
│          Orchestration & IO         │
└──────────────┬──────────────────────┘
               │ Coordinates pure + IO
┌──────────────┼──────────────────────┐
│   Database   │    Network/API       │
│   Adapter    │    Adapter           │
│     (IO)     │      (IO)            │
│              │                      │
│ loadData     │  fetchPrices         │
│ saveResults  │  executeOrders       │
└──────────────┼──────────────────────┘
               │ Provides data to pure core
┌──────────────┴──────────────────────┐
│         Pure Core Logic             │
│                                     │
│ • Trader.Strategy.*                 │
│ • Trader.Indicator.*                │
│ • Trader.Technical.*                │
│ • Trading calculations              │
│ • Signal generation                 │
│                                     │
│ NO IO! Easy to test & reason about  │
└─────────────────────────────────────┘
```

#### Implementation Guidelines
```haskell
-- ✅ CORE: Pure business logic
module Trader.Strategy.BollingerBands where

calculateSignal :: BollingerBands -> Price -> TradingSignal
calculateSignal bands price =
  if price > upperBand bands
    then Sell Strong "Price above upper band"
    else if price < lowerBand bands
    then Buy Strong "Price below lower band"
    else Hold "Price within bands"

-- ✅ EDGE: IO operations
module Trader.Data.Provider where

loadMarketData :: Symbol -> DateRange -> IO [Price]
loadMarketData symbol range = do
  -- Database/API calls here

-- ✅ MAIN: Coordination
main :: IO ()
main = do
  prices <- loadMarketData "EURUSD" lastMonth  -- IO edge
  let signals = map (calculateSignal config) prices  -- Pure core
  mapM_ executeOrder signals  -- IO edge
```

### 📂 Module Organization Pattern

```haskell
-- 🎯 DOMAIN MODELS: Pure data types
module Trader.Types where
data TradingSignal = Buy | Sell | Hold
data Price = Price Double
data Period = Period Int

-- 🧮 BUSINESS LOGIC: Pure calculations
module Trader.Strategy.MovingAverage where
calculateMA :: Period -> [Price] -> Maybe Price

module Trader.Indicator.RSI where
calculateRSI :: Period -> [Price] -> Maybe Double

-- 🌐 EFFECTFUL EDGES: IO operations
module Trader.Data.Kraken where
fetchPrices :: Symbol -> IO [Price]

module Trader.Execution.Orders where
executeOrder :: TradingSignal -> IO OrderResult

-- 🎭 ORCHESTRATION: Wiring together
module Main where  -- app/Main.hs
main = do
  prices <- fetchPrices "EURUSD"          -- IO
  let ma = calculateMA (Period 20) prices  -- Pure
  result <- executeOrder (generateSignal ma)  -- IO
```

### 🔗 Type-Safe Configuration Pattern

```haskell
-- ✅ GOOD: Type-safe configuration with validation
data StrategyConfig = StrategyConfig
  { _period :: Period
  , _threshold :: Threshold
  , _riskLevel :: RiskLevel
  } deriving (Show, Eq)

-- Smart constructors prevent invalid states
mkPeriod :: Int -> Either ConfigError Period
mkPeriod n
  | n > 0 = Right (Period n)
  | otherwise = Left (InvalidPeriod n)

mkThreshold :: Double -> Either ConfigError Threshold
mkThreshold t
  | t >= 0.0 && t <= 1.0 = Right (Threshold t)
  | otherwise = Left (InvalidThreshold t)

-- Type-safe config construction
mkConfig :: Int -> Double -> Double -> Either ConfigError StrategyConfig
mkConfig p t r = StrategyConfig
  <$> mkPeriod p
  <*> mkThreshold t
  <*> mkRiskLevel r
```

---

## 💎 CODE QUALITY GUIDELINES

### Function Design
**Small, focused, composable functions**

```haskell
-- ✅ Good: Single responsibility
calculateRSI :: Int -> [Double] -> Maybe Double
calculateRSI period prices = do
  guard (period > 0 && length prices >= period)
  let changes = zipWith (-) (tail prices) prices
  let (gains, losses) = partition (>= 0) changes
  avgGain <- safeAverage gains
  avgLoss <- safeAverage (map abs losses)
  guard (avgLoss > 0)
  return (100 - (100 / (1 + avgGain / avgLoss)))

-- Helper function
safeAverage :: [Double] -> Maybe Double
safeAverage [] = Nothing
safeAverage xs = Just (sum xs / fromIntegral (length xs))
```

### Error Handling
**Explicit error handling with types**

```haskell
-- ✅ Good: Explicit error types
data IndicatorError
  = InsufficientData Int Int  -- required, available
  | InvalidPeriod Int
  | NoValidPrices
  deriving (Show, Eq)

calculateIndicator :: Int -> [Double] -> Either IndicatorError Double
calculateIndicator period prices
  | period <= 0 = Left (InvalidPeriod period)
  | length prices < period = Left (InsufficientData period (length prices))
  | null validPrices = Left NoValidPrices
  | otherwise = Right result
  where
    validPrices = filter (> 0) prices
    result = calculateFromValid validPrices
```

### Documentation Standards
**Comprehensive Haddock documentation**

```haskell
-- | Calculate Relative Strength Index for given period and price data.
--
-- RSI oscillates between 0 and 100, where values above 70 typically indicate
-- overbought conditions and values below 30 indicate oversold conditions.
--
-- ==== __Examples__
--
-- >>> calculateRSI 14 [44.0, 44.5, 44.9, 44.5, 44.6]
-- Just 72.45
--
-- >>> calculateRSI 0 [44.0, 44.5]  -- Invalid period
-- Nothing
--
-- @since 0.1.0.0
calculateRSI :: Int     -- ^ Period for RSI calculation (must be > 0)
             -> [Double] -- ^ Price data (must have at least 'period' elements)
             -> Maybe Double -- ^ RSI value between 0-100, or Nothing if invalid input
```

---

## 🧪 TESTING STRATEGY

### Property-Based Testing
**Use QuickCheck for mathematical properties**

```haskell
prop_rsiRange :: Positive Int -> [Positive Double] -> Property
prop_rsiRange (Positive period) prices' =
  let prices = map getPositive prices'
  in length prices >= period ==>
     case calculateRSI period prices of
       Just rsi -> rsi >= 0 && rsi <= 100
       Nothing -> False

prop_movingAverageMonotonic :: [OrderedList Double] -> Property
prop_movingAverageMonotonic prices' =
  let prices = concatMap getOrdered prices'
      ma1 = calculateMA 5 prices
      ma2 = calculateMA 10 prices
  in length prices >= 10 ==>
     fromMaybe 0 ma1 <= fromMaybe 0 ma2  -- Shorter MA more responsive
```

### Integration Testing
**Test real workflows with actual data**

```haskell
testBollingerBandsStrategy :: Spec
testBollingerBandsStrategy = describe "BollingerBands Strategy" $ do
  it "generates valid signals with real market data" $ do
    -- Use actual market data from Trader.Data modules
    prices <- loadRealMarketData "EURUSD" (days 30)
    let config = BollingerBandsConfig 20 2.0
    signals <- runStrategy config prices
    -- Validate signal properties
    all isValidSignal signals `shouldBe` True
    length signals `shouldBe` length prices - 19  -- Account for indicator lag
```

---

## 🚀 PERFORMANCE CONSIDERATIONS

### Lazy Evaluation Benefits
**Leverage laziness for efficient data processing**

```haskell
-- ✅ Good: Lazy infinite stream processing
movingAverages :: Int -> [Double] -> [Double]
movingAverages n prices =
  map (average . take n) (tails prices)
  where average xs = sum xs / fromIntegral (length xs)

-- Process only what's needed
recentAverages = take 10 $ movingAverages 20 infinitePriceStream
```

### Space Efficiency
**Avoid space leaks in accumulations**

```haskell
-- ✅ Good: Strict accumulation
calculateRunningSum :: [Double] -> [Double]
calculateRunningSum = scanl1' (+)  -- scanl1' is strict

-- ❌ Avoid: Lazy accumulation can cause space leaks
calculateRunningSum = scanl1 (+)   -- Lazy, may leak memory
```

---

## 📋 DEVELOPMENT WORKFLOW

### Before Starting Any Task
1. **Read Instructions**: Review this entire document
2. **Understand Context**: Read existing code and architecture
3. **Plan Approach**: Break down complex tasks into smaller functions
4. **Check Dependencies**: Ensure required modules and data are available

### During Implementation
1. **Type-First**: Define types before implementing functions
2. **Test Early**: Write tests alongside implementation
3. **Document**: Add Haddock comments for all public functions
4. **Validate**: Ensure code compiles and tests pass

### After Completion
1. **Update Documentation**: Update task summaries and plans
2. **Run Tests**: Verify all tests pass with `stack test :TEST_EXE  --fast`, where `TEST_EXE` is the name of the test executable found in the `package.yaml` file.
3. **Clean Up**: Remove compilation artifacts
4. **Request Feedback**: Use mandatory feedback loop

---

## ⚡ COMMON PATTERNS AND IDIOMS

### Safe List Operations
```haskell
-- ✅ Good: Safe head/tail operations
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
```

### Configuration Pattern
```haskell
-- ✅ Good: Configuration with smart constructors
data StrategyConfig = StrategyConfig
  { _period :: Period
  , _threshold :: Threshold
  , _riskLevel :: RiskLevel
  } deriving (Show, Eq)

makeClassy ''StrategyConfig

-- Smart constructor with validation
mkStrategyConfig :: Int -> Double -> Double -> Either ConfigError StrategyConfig
mkStrategyConfig p t r = StrategyConfig
  <$> mkPeriod p
  <*> mkThreshold t
  <*> mkRiskLevel r
```

### Monad Transformer Stacks
```haskell
-- ✅ Good: Clean monad stack for complex operations
type TradingM = ReaderT TradingConfig (ExceptT TradingError IO)

runTrading :: TradingConfig -> TradingM a -> IO (Either TradingError a)
runTrading config action = runExceptT (runReaderT action config)
```

---

## 🎯 SUCCESS METRICS

### Code Quality Indicators
- ✅ All functions have explicit type signatures
- ✅ No partial functions in production code
- ✅ Comprehensive Haddock documentation
- ✅ All tests pass with 100% success rate
- ✅ No compilation warnings

### Architecture Quality
- ✅ Pure core with effectful edges
- ✅ Type-safe configuration management
- ✅ Composable, modular design
- ✅ Clear separation of concerns

### Development Process
- ✅ Tasks completed according to documentation requirements
- ✅ User feedback properly integrated
- ✅ Plans and reports maintained and updated
- ✅ Quality gates met before feedback requests

---

## 🔧 TROUBLESHOOTING

### Common Issues and Solutions

**Build Failures**:
- Check `package.yaml` syntax
- Verify Stack resolver compatibility
- Clean and rebuild: `stack clean && stack build`

**Test Failures**:
- Check for missing real implementations
- Verify market data availability
- Ensure proper imports and dependencies

**Type Errors**:
- Use type holes (`_`) to get compiler guidance
- Check type family instances
- Verify newtype/data constructor usage

**Memory Issues**:
- Profile with `+RTS -h`
- Check for space leaks in lazy operations
- Use strict data structures when appropriate

---

## 📖 CONTINUOUS IMPROVEMENT

Track improvements in `IMPROVEMENT_IDEAS.md`:

### Documentation Template
```markdown
## [Issue Area]: [Brief Description]

**Current Problem**: [Detailed description of the issue]

**Proposed Solution**: [Specific improvement recommendation]

**Benefits**: [Why this improvement is valuable]

**Implementation**: [How to implement the improvement]

**References**: [Relevant documentation or examples]
```

---

**Remember**: These guidelines ensure consistent, high-quality Haskell code that is maintainable, testable, and follows functional programming best practices. Always prioritize clarity and correctness over cleverness.
