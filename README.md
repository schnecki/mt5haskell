# mt5haskell

Haskell library for MetaTrader 5 trading on Linux using Wine.

Supports dual communication channels:
- **PythonBridge**: Complete data via mt5linux Python server
- **FileBridge**: Direct EA communication (faster, minimal data)

## Features

- 🚀 **Dual Channel Routing**: Choose between Python (complete data) or File bridge (speed)
- 🔒 **Type-Safe**: Comprehensive Haskell types for all MT5 data structures
- ⚡ **Error Handling**: Automatic retry logic with exponential backoff
- 📊 **Complete API**: Account info, positions, orders, symbols, order execution
- 🧪 **Well-Tested**: 183 tests (179 passing), comprehensive test coverage

## Quick Start

### Installation

1. **Install Wine**:
   ```bash
   # Arch Linux
   sudo pacman -S wine

   # Ubuntu/Debian
   sudo apt install wine64
   ```

2. **Install Python for Windows** (via Wine):
   ```bash
   # Download from python.org
   wine python-installer.exe

   # Find python.exe path (usually):
   ~/.wine/drive_c/users/$USER/Local Settings/Application Data/Programs/Python/Python39/python.exe
   ```

3. **Install MetaTrader5 Python library**:
   ```bash
   # In Wine Python environment
   wine python.exe -m pip install MetaTrader5
   wine python.exe -m pip install --upgrade MetaTrader5
   ```

4. **Install mt5linux** (Python package):
   ```bash
   # On both Windows Python (Wine) and Linux Python
   wine python.exe -m pip install mt5linux  # Windows side
   pip install mt5linux                       # Linux side
   ```

5. **Add mt5haskell to your project**:
   ```yaml
   # stack.yaml or package.yaml
   dependencies:
     - mt5haskell
   ```

### Usage

#### 1. Start mt5linux Python Server

```bash
# In a separate terminal
python -m mt5linux /path/to/wine/python.exe
# Default: localhost:18812
```

#### 2. Start MT5 Terminal & Attach EA

1. Open MT5: `wine terminal64.exe`
2. Open any chart (e.g., EURUSD)
3. Drag `MT5RestAPIBridge.mq5` EA onto chart
4. Verify EA is running (Experts tab)

#### 3. Use in Haskell

```haskell
{-# LANGUAGE OverloadedStrings #-}

import MT5.Init (startMT5)
import MT5.Config (defaultMT5Config)
import MT5.API (accountInfo, positionsGet, symbolInfo, orderSend)

main :: IO ()
main = do
  -- Initialize MT5 (starts terminal if needed)
  _ <- startMT5 defaultMT5Config

  -- Get account information
  account <- accountInfo
  print account

  -- Get open positions
  positions <- positionsGet
  print positions

  -- Get symbol information
  eurusd <- symbolInfo "EURUSD"
  print eurusd

  -- Place an order (see MT5.Data.MqlTradeRequest for details)
  -- result <- orderSend myRequest
  -- print result
```

## Configuration Examples

See [plans/CONFIG_EXAMPLES.md](plans/CONFIG_EXAMPLES.md) for detailed configuration examples.

### PythonBridge (Default - Complete Data)

```haskell
import MT5.Config (defaultMT5Config, withPythonBridge)
import MT5.Init (startMT5)

main = startMT5 $ withPythonBridge defaultMT5Config
```

### FileBridge (Faster, Minimal Data)

```haskell
import MT5.Config (defaultMT5Config, withFileBridge)
import MT5.Init (startMT5)

main = startMT5 $ withFileBridge defaultMT5Config
```

### Recommended Production Setup

```haskell
-- Use PythonBridge for data retrieval (complete data)
-- FileBridge automatically used for orderSend (broker requirement)
import MT5.Config (defaultMT5Config, withPythonBridge)

main = startMT5 $ withPythonBridge defaultMT5Config
```

## Data Completeness Warning

⚠️ **FileBridge has significant data loss**:
- accountInfo: 50% data loss (7 vs 14 fields)
- positionsGet: 37% data loss (9 vs 14 fields)
- **symbolInfo**: **93% data loss** (7 vs 104 fields) - NEVER use FileBridge for symbolInfo!
- ordersGet: 41% data loss (10 vs 17 fields)

**Recommendation**: Use PythonBridge by default (complete data), FileBridge only when performance critical.

## Error Handling with ExceptT

As of version 0.2.0.0, the API uses `ExceptT MT5Error IO` monad transformer for type-safe error handling with automatic error propagation.

### Basic Usage

```haskell
import MT5.API (positionsGet, ordersGet)
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  -- Use runExceptT to execute ExceptT actions
  result <- runExceptT positionsGet
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right positions -> print positions
```

### Composing Operations

The key benefit of `ExceptT` is automatic error propagation - no need for manual case matching:

```haskell
import MT5.API (positionsGet, ordersGet, accountInfo)
import Control.Monad.Except (runExceptT)

-- Errors propagate automatically through the do-block
tradingReport :: ExceptT MT5Error IO TradingReport
tradingReport = do
  account <- accountInfo         -- Automatically stops on error
  positions <- positionsGet      -- Only runs if accountInfo succeeded
  orders <- ordersGet Nothing Nothing
  return $ TradingReport account positions orders

main :: IO ()
main = do
  result <- runExceptT tradingReport
  case result of
    Left err -> putStrLn $ "Failed to generate report: " ++ show err
    Right report -> print report
```

### Compatibility Wrappers

For gradual migration, compatibility wrappers returning `Either` are provided:

```haskell
import MT5.API (positionsGetEither, ordersGetEither)

main :: IO ()
main = do
  -- Old style - returns IO (Either MT5Error [TradePosition])
  result <- positionsGetEither
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right positions -> print positions
```

### Helper Functions

```haskell
import MT5.API (runWithDefault, runWithLogging)

main :: IO ()
main = do
  -- Return default value on error
  positions <- runWithDefault [] positionsGet
  print positions

  -- Log errors and return Maybe
  mAccount <- runWithLogging accountInfo
  case mAccount of
    Nothing -> putStrLn "Failed to get account info"
    Just account -> print account
```

### Error Types

All operations can fail with these error types:

- `TimeoutError Text Int` - Request timed out (operation name, timeout ms)
- `ParseError Text Text` - Failed to parse MT5 response (type name, details)
- `BrokerError TradeRetcode Text` - MT5 broker error (return code, message)
- `PythonProcessError Text` - Python bridge communication failed

### Migration from Either

**Before (v0.1.x)**:
```haskell
main = do
  result <- positionsGet  -- Returns IO (Either MT5Error [TradePosition])
  case result of
    Left err -> ...
    Right positions -> ...
```

**After (v0.2.0+)**:
```haskell
main = do
  result <- runExceptT positionsGet  -- ExceptT MT5Error IO [TradePosition]
  case result of
    Left err -> ...
    Right positions -> ...
```

### Automatic Retry with ExceptT

For retry logic, compose with retry utilities:

```haskell
import MT5.Error (retryWithBackoff, defaultRetryConfig)
import Control.Monad.Except (runExceptT)

main = do
  -- Wrap ExceptT action with retry logic
  result <- retryWithBackoff defaultRetryConfig $ runExceptT positionsGet
  case result of
    RetrySuccess positions -> print positions
    RetryFailure err -> putStrLn $ "Failed after retries: " ++ show err
```

## API Reference

### MT5.API

**Core Functions** (ExceptT-based):
- `positionsGet :: ExceptT MT5Error IO [TradePosition]`
- `ordersGet :: Maybe Symbol -> Maybe Ticket -> ExceptT MT5Error IO [TradeOrder]`
- `accountInfo :: ExceptT MT5Error IO AccountInfo`
- `symbolInfo :: Symbol -> ExceptT MT5Error IO SymbolInfo`
- `orderSend :: MqlTradeRequest -> ExceptT MT5Error IO OrderSendResult`

**Compatibility Wrappers** (Either-based):
- `positionsGetEither :: IO (Either MT5Error [TradePosition])`
- `ordersGetEither :: Maybe Symbol -> Maybe Ticket -> IO (Either MT5Error [TradeOrder])`
- `orderSendEither :: MqlTradeRequest -> IO (Either MT5Error OrderSendResult)`

**Helper Functions**:
- `liftMaybe :: MT5Error -> Maybe a -> ExceptT MT5Error IO a`
- `eitherToExceptT :: Either MT5Error a -> ExceptT MT5Error IO a`
- `maybeToExceptT :: MT5Error -> Maybe a -> ExceptT MT5Error IO a`
- `runWithDefault :: a -> ExceptT e IO a -> IO a`
- `runWithLogging :: Show e => ExceptT e IO a -> IO (Maybe a)`

### MT5.Config

- `defaultMT5Config :: Config`
- `withPythonBridge :: Config -> Config`
- `withFileBridge :: Config -> Config`
- `getConfig :: IO Config`
- `setConfig :: Config -> IO ()`

### MT5.Error

- `handleMT5Error :: IO a -> IO (Either MT5Error a)`
- `withTimeout :: Int -> Text -> IO a -> IO (Either MT5Error a)`
- `retryWithBackoff :: RetryConfig -> IO (Either MT5Error a) -> IO (RetryResult a)`
- `safeExecute :: RetryConfig -> Int -> Text -> IO a -> IO (Either MT5Error a)`

## Building & Testing

```bash
# Build
stack build --fast

# Run tests (package only, not dependencies)
stack test :mt5haskell-test --fast

# Generate documentation
stack haddock --open mt5haskell
```

## Project Structure

```
mt5haskell/
├── src/
│   └── MT5/
│       ├── API.hs              # Main API functions
│       ├── Config.hs           # Configuration
│       ├── Error.hs            # Error handling
│       ├── Init.hs             # Initialization
│       ├── Communication/      # Communication modules
│       └── Data/               # MT5 data types
├── test/
│   └── MT5/
│       ├── Integration/        # Integration tests
│       ├── APISpec.hs          # API tests
│       └── ...                 # Unit tests
├── examples/
│   └── TestFileComm.hs         # Example usage
├── plans/
│   ├── CONFIG_EXAMPLES.md      # Configuration examples
│   └── TROUBLESHOOTING.md      # Troubleshooting guide
└── README.md                   # This file
```

## Troubleshooting

See [plans/TROUBLESHOOTING.md](plans/TROUBLESHOOTING.md) for detailed troubleshooting guide.

### Common Issues

**"Expected bytestring from python"**: mt5linux Python server not running
**"Timeout error"**: MT5 EA not attached to chart
**"File access error"**: Check Wine permissions and file paths

## License

[Your License Here]

## Credits

Based on [mt5linux](https://github.com/lucas-campagna/mt5linux) by Lucas Campagna for Python bridge support.

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `stack test :mt5haskell-test --fast`
5. Submit a pull request

## Contact

Manuel Schneckenreither
