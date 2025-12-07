# Changelog for `mt5haskell`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.2.0.0 - 2024-12-05

### Changed
- **BREAKING**: Migrated core API functions to `ExceptT MT5Error IO` monad transformer for type-safe error handling
- Function signature changes:
  - `positionsGet :: IO (Either MT5Error [TradePosition])` → `ExceptT MT5Error IO [TradePosition]`
  - `ordersGet :: Maybe Symbol -> Maybe Ticket -> IO (Either MT5Error [TradeOrder])` → `ExceptT MT5Error IO [TradeOrder]`
  - `orderSend :: MqlTradeRequest -> IO (Either MT5Error OrderSendResult)` → `ExceptT MT5Error IO OrderSendResult`
  - `accountInfo :: IO (Either MT5Error AccountInfo)` → `ExceptT MT5Error IO AccountInfo`
  - `symbolInfo :: Symbol -> IO (Either MT5Error SymbolInfo)` → `ExceptT MT5Error IO SymbolInfo`
  - `positionClose :: Ticket -> IO (Either MT5Error Bool)` → `ExceptT MT5Error IO Bool`
  - `positionClosePartial :: Ticket -> Double -> IO (Either MT5Error Bool)` → `ExceptT MT5Error IO Bool`
  - `positionModify :: Ticket -> Double -> Double -> IO (Either MT5Error Bool)` → `ExceptT MT5Error IO Bool`
- Eliminated use of `error` function in favor of explicit error types (`TimeoutError`, `ParseError`, `BrokerError`, `PythonProcessError`, `ValidationError`)

### Added
- Helper functions for ExceptT:
  - `liftMaybe :: MT5Error -> Maybe a -> ExceptT MT5Error IO a`
  - `eitherToExceptT :: Either MT5Error a -> ExceptT MT5Error IO a`
  - `maybeToExceptT :: MT5Error -> Maybe a -> ExceptT MT5Error IO a`
- Compatibility wrappers for gradual migration:
  - `positionsGetEither :: IO (Either MT5Error [TradePosition])`
  - `ordersGetEither :: Maybe Symbol -> Maybe Ticket -> IO (Either MT5Error [TradeOrder])`
  - `orderSendEither :: MqlTradeRequest -> IO (Either MT5Error OrderSendResult)`
  - `accountInfoEither :: IO (Either MT5Error AccountInfo)`
  - `symbolInfoEither :: Symbol -> IO (Either MT5Error SymbolInfo)`
  - `positionCloseEither :: Ticket -> IO (Either MT5Error Bool)`
  - `positionClosePartialEither :: Ticket -> Double -> IO (Either MT5Error Bool)`
  - `positionModifyEither :: Ticket -> Double -> Double -> IO (Either MT5Error Bool)`
  - `runWithDefault :: a -> ExceptT e IO a -> IO a`
  - `runWithLogging :: Show e => ExceptT e IO a -> IO (Maybe a)`
- Comprehensive Haddock documentation with ExceptT usage examples and composition patterns
- Dependencies: `mtl` and `transformers` packages

### Migration Guide

**Before (v0.1.x)**:
```haskell
main = do
  result <- positionsGet  -- IO (Either MT5Error [TradePosition])
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right positions -> print positions
```

**After (v0.2.0)**:

*Option 1 - Use ExceptT directly (recommended)*:
```haskell
import Control.Monad.Except (runExceptT)

main = do
  result <- runExceptT positionsGet  -- ExceptT MT5Error IO [TradePosition]
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right positions -> print positions
```

*Option 2 - Use compatibility wrapper*:
```haskell
main = do
  result <- positionsGetEither  -- IO (Either MT5Error [TradePosition])
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right positions -> print positions
```

### Benefits of ExceptT

- **Composability**: Chain multiple operations without manual error handling
- **Automatic Propagation**: Errors propagate automatically through do-blocks
- **Type Safety**: Compile-time guarantees about error handling
- **No Exceptions**: Eliminates runtime exceptions from `error` function

See README.md for complete migration guide and examples.

## 0.1.0.0 - YYYY-MM-DD
