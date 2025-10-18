{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive error handling for MT5 operations.
--
-- This module provides type-safe error handling with specific error categories,
-- MT5 error code mappings, and recovery strategies.
--
-- ==== __Error Categories__
--
-- * Connection errors (MT5 not running, Wine issues)
-- * Communication errors (timeout, file access, Python process)
-- * Broker errors (MT5 trade retcodes, account restrictions)
-- * Data errors (parsing failures, invalid responses)
-- * Validation errors (invalid parameters, business logic violations)
--
-- ==== __Usage Examples__
--
-- >>> handleMT5Error $ accountInfo
-- Right (AccountInfo {...})
--
-- >>> handleMT5Error $ orderSend invalidRequest
-- Left (BrokerError TRADE_RETCODE_INVALID_PRICE "Price is not valid")
--
module MT5.Error
  ( -- * Error Types
    MT5Error (..)
  , ErrorCategory (..)
  , ErrorSeverity (..)
    
    -- * MT5 Error Code Mapping
  , MT5ErrorCode (..)
  , fromMT5ErrorCode
  , toMT5ErrorCode
  , errorCodeDescription
    
    -- * Error Handling
  , handleMT5Error
  , retryWithBackoff
  , withTimeout
  , safeExecute
    
    -- * Retry Strategies
  , RetryConfig (..)
  , defaultRetryConfig
  , RetryResult (..)
    
    -- * Error Recovery
  , RecoveryStrategy (..)
  , applyRecovery
    
    -- * Utilities
  , isFatalError
  , isRetryableError
  , getErrorCategory
  , getErrorSeverity
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (Exception, SomeException, catch, try)
import           Control.Monad            (when)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Typeable            (Typeable)
import           EasyLogger               (logWarning)
import           GHC.Generics             (Generic)
import           MT5.Data.OrderSendResult (TradeRetcode (..))
import           System.Timeout           (timeout)


-- | Comprehensive MT5 error type covering all failure scenarios.
data MT5Error
  = -- Connection Errors
    ConnectionError Text  -- ^ MT5 not running, Wine not found, initialization failed
  | LoginError Text Int   -- ^ Login failed (error message, error code)
    
    -- Communication Errors  
  | TimeoutError Text Int -- ^ Operation timed out (operation name, milliseconds waited)
  | FileAccessError Text  -- ^ File I/O error (EA bridge files)
  | PythonProcessError Text -- ^ Python process communication error
  | ChannelError Text     -- ^ Communication channel error (neither Python nor File works)
    
    -- Broker Errors (MT5 Trade Retcodes)
  | BrokerError TradeRetcode Text -- ^ MT5 trade retcode with description
  | InsufficientFunds Double Double -- ^ Not enough money (required, available)
  | InvalidVolume Double Text -- ^ Invalid lot size (volume, reason)
  | InvalidPrice Double Text  -- ^ Invalid price (price, reason)
  | TradingDisabled Text  -- ^ Trading not allowed (reason)
  | MarketClosed Text     -- ^ Market is closed (symbol)
    
    -- Data Errors
  | ParseError Text Text  -- ^ Failed to parse response (field name, raw data)
  | InvalidResponse Text  -- ^ Response structure invalid (details)
  | MissingField Text     -- ^ Required field missing (field name)
  | DataLossWarning Text Int Int -- ^ Data loss from EA (function, fields received, fields expected)
    
    -- Validation Errors
  | ValidationError Text  -- ^ Business logic validation failed
  | InvalidParameter Text Text -- ^ Invalid parameter (parameter name, reason)
  | UnsupportedOperation Text -- ^ Operation not supported (operation name)
    
    -- System Errors
  | UnknownError Text     -- ^ Unexpected error
  | InternalError Text    -- ^ Internal library error
  deriving stock (Show, Eq, Generic, Typeable)

instance Exception MT5Error


-- | Error category for classification and handling strategies.
data ErrorCategory
  = CategoryConnection   -- ^ Connection and initialization errors
  | CategoryCommunication -- ^ Communication and timeout errors
  | CategoryBroker       -- ^ Broker and trading errors
  | CategoryData         -- ^ Data parsing and validation errors
  | CategoryValidation   -- ^ Parameter validation errors
  | CategorySystem       -- ^ System and internal errors
  deriving stock (Show, Eq, Generic)


-- | Error severity for logging and alerting.
data ErrorSeverity
  = SeverityCritical  -- ^ Critical error, system cannot continue
  | SeverityHigh      -- ^ High severity, operation failed
  | SeverityMedium    -- ^ Medium severity, retryable
  | SeverityLow       -- ^ Low severity, warning
  deriving stock (Show, Eq, Ord, Generic)


-- | MT5 system error codes (beyond trade retcodes).
data MT5ErrorCode
  = MT5_SUCCESS                -- ^ 1: Success
  | MT5_FAIL                   -- ^ -1: Generic failure
  | MT5_NOT_INITIALIZED        -- ^ -2: MT5 not initialized
  | MT5_INVALID_PARAMS         -- ^ -10: Invalid parameters
  | MT5_NO_MEMORY              -- ^ -20: Not enough memory
  | MT5_FILE_NOT_FOUND         -- ^ -100: File not found
  | MT5_FILE_CANNOT_OPEN       -- ^ -101: Cannot open file
  | MT5_FILE_WRITE_ERROR       -- ^ -102: File write error
  | MT5_TIMEOUT                -- ^ -200: Operation timeout
  | MT5_UNAUTHORIZED           -- ^ -300: Authorization failed
  | MT5_TRADE_DISABLED         -- ^ -400: Trading disabled
  | MT5_MARKET_CLOSED          -- ^ -401: Market is closed
  | MT5_NO_CONNECTION          -- ^ -500: No connection to server
  | MT5_UNKNOWN_ERROR Int      -- ^ Unknown error code
  deriving stock (Show, Eq, Generic)


-- | Convert integer error code to MT5ErrorCode.
fromMT5ErrorCode :: Int -> MT5ErrorCode
fromMT5ErrorCode 1    = MT5_SUCCESS
fromMT5ErrorCode (-1) = MT5_FAIL
fromMT5ErrorCode (-2) = MT5_NOT_INITIALIZED
fromMT5ErrorCode (-10) = MT5_INVALID_PARAMS
fromMT5ErrorCode (-20) = MT5_NO_MEMORY
fromMT5ErrorCode (-100) = MT5_FILE_NOT_FOUND
fromMT5ErrorCode (-101) = MT5_FILE_CANNOT_OPEN
fromMT5ErrorCode (-102) = MT5_FILE_WRITE_ERROR
fromMT5ErrorCode (-200) = MT5_TIMEOUT
fromMT5ErrorCode (-300) = MT5_UNAUTHORIZED
fromMT5ErrorCode (-400) = MT5_TRADE_DISABLED
fromMT5ErrorCode (-401) = MT5_MARKET_CLOSED
fromMT5ErrorCode (-500) = MT5_NO_CONNECTION
fromMT5ErrorCode code  = MT5_UNKNOWN_ERROR code


-- | Convert MT5ErrorCode to integer.
toMT5ErrorCode :: MT5ErrorCode -> Int
toMT5ErrorCode MT5_SUCCESS = 1
toMT5ErrorCode MT5_FAIL = -1
toMT5ErrorCode MT5_NOT_INITIALIZED = -2
toMT5ErrorCode MT5_INVALID_PARAMS = -10
toMT5ErrorCode MT5_NO_MEMORY = -20
toMT5ErrorCode MT5_FILE_NOT_FOUND = -100
toMT5ErrorCode MT5_FILE_CANNOT_OPEN = -101
toMT5ErrorCode MT5_FILE_WRITE_ERROR = -102
toMT5ErrorCode MT5_TIMEOUT = -200
toMT5ErrorCode MT5_UNAUTHORIZED = -300
toMT5ErrorCode MT5_TRADE_DISABLED = -400
toMT5ErrorCode MT5_MARKET_CLOSED = -401
toMT5ErrorCode MT5_NO_CONNECTION = -500
toMT5ErrorCode (MT5_UNKNOWN_ERROR code) = code


-- | Get human-readable description for MT5 error code.
errorCodeDescription :: MT5ErrorCode -> Text
errorCodeDescription MT5_SUCCESS = "Operation completed successfully"
errorCodeDescription MT5_FAIL = "Operation failed"
errorCodeDescription MT5_NOT_INITIALIZED = "MT5 terminal not initialized"
errorCodeDescription MT5_INVALID_PARAMS = "Invalid parameters provided"
errorCodeDescription MT5_NO_MEMORY = "Not enough memory to complete operation"
errorCodeDescription MT5_FILE_NOT_FOUND = "Required file not found"
errorCodeDescription MT5_FILE_CANNOT_OPEN = "Cannot open file"
errorCodeDescription MT5_FILE_WRITE_ERROR = "Error writing to file"
errorCodeDescription MT5_TIMEOUT = "Operation timed out"
errorCodeDescription MT5_UNAUTHORIZED = "Authorization failed"
errorCodeDescription MT5_TRADE_DISABLED = "Trading is disabled"
errorCodeDescription MT5_MARKET_CLOSED = "Market is closed"
errorCodeDescription MT5_NO_CONNECTION = "No connection to trade server"
errorCodeDescription (MT5_UNKNOWN_ERROR code) = T.pack $ "Unknown error code: " ++ show code


-- | Retry configuration for automatic retry with exponential backoff.
data RetryConfig = RetryConfig
  { retryMaxAttempts   :: Int    -- ^ Maximum number of retry attempts
  , retryInitialDelay  :: Int    -- ^ Initial delay in milliseconds
  , retryMaxDelay      :: Int    -- ^ Maximum delay in milliseconds
  , retryBackoffFactor :: Double -- ^ Exponential backoff multiplier (e.g., 2.0 for doubling)
  , retryableErrors    :: MT5Error -> Bool -- ^ Predicate to determine if error is retryable
  }

instance Show RetryConfig where
  show config = "RetryConfig {maxAttempts=" ++ show (retryMaxAttempts config) ++
                ", initialDelay=" ++ show (retryInitialDelay config) ++
                ", maxDelay=" ++ show (retryMaxDelay config) ++
                ", backoffFactor=" ++ show (retryBackoffFactor config) ++ "}"


-- | Default retry configuration with exponential backoff.
--
-- - Max 3 attempts
-- - Initial delay: 100ms
-- - Max delay: 5000ms (5 seconds)
-- - Backoff factor: 2.0 (doubling)
-- - Retryable: timeout and communication errors
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { retryMaxAttempts = 3
  , retryInitialDelay = 100
  , retryMaxDelay = 5000
  , retryBackoffFactor = 2.0
  , retryableErrors = isRetryableError
  }


-- | Result of retry operation with attempt tracking.
data RetryResult a
  = RetrySuccess a Int        -- ^ Success (result, attempts made)
  | RetryFailed MT5Error Int  -- ^ Failed after retries (last error, attempts made)
  | RetryGaveUp MT5Error Int  -- ^ Gave up (non-retryable error, attempt number)
  deriving stock (Show, Eq, Generic)


-- | Recovery strategy for handling errors.
data RecoveryStrategy
  = RetryOperation RetryConfig -- ^ Retry with exponential backoff
  | FallbackToPython           -- ^ Fall back to Python bridge
  | FallbackToFile             -- ^ Fall back to File bridge
  | UseDefaultValue            -- ^ Use safe default value
  | PropagateError             -- ^ Propagate error to caller
  | LogAndContinue             -- ^ Log error but continue execution
  deriving stock (Show, Generic)


-- | Classify error into category.
getErrorCategory :: MT5Error -> ErrorCategory
getErrorCategory (ConnectionError _) = CategoryConnection
getErrorCategory (LoginError _ _) = CategoryConnection
getErrorCategory (TimeoutError _ _) = CategoryCommunication
getErrorCategory (FileAccessError _) = CategoryCommunication
getErrorCategory (PythonProcessError _) = CategoryCommunication
getErrorCategory (ChannelError _) = CategoryCommunication
getErrorCategory (BrokerError _ _) = CategoryBroker
getErrorCategory (InsufficientFunds _ _) = CategoryBroker
getErrorCategory (InvalidVolume _ _) = CategoryBroker
getErrorCategory (InvalidPrice _ _) = CategoryBroker
getErrorCategory (TradingDisabled _) = CategoryBroker
getErrorCategory (MarketClosed _) = CategoryBroker
getErrorCategory (ParseError _ _) = CategoryData
getErrorCategory (InvalidResponse _) = CategoryData
getErrorCategory (MissingField _) = CategoryData
getErrorCategory (DataLossWarning _ _ _) = CategoryData
getErrorCategory (ValidationError _) = CategoryValidation
getErrorCategory (InvalidParameter _ _) = CategoryValidation
getErrorCategory (UnsupportedOperation _) = CategoryValidation
getErrorCategory (UnknownError _) = CategorySystem
getErrorCategory (InternalError _) = CategorySystem


-- | Determine error severity.
getErrorSeverity :: MT5Error -> ErrorSeverity
getErrorSeverity (ConnectionError _) = SeverityCritical
getErrorSeverity (LoginError _ _) = SeverityCritical
getErrorSeverity (TimeoutError _ _) = SeverityMedium
getErrorSeverity (FileAccessError _) = SeverityHigh
getErrorSeverity (PythonProcessError _) = SeverityHigh
getErrorSeverity (ChannelError _) = SeverityCritical
getErrorSeverity (BrokerError TRADE_RETCODE_DONE _) = SeverityLow
getErrorSeverity (BrokerError _ _) = SeverityHigh
getErrorSeverity (InsufficientFunds _ _) = SeverityHigh
getErrorSeverity (InvalidVolume _ _) = SeverityMedium
getErrorSeverity (InvalidPrice _ _) = SeverityMedium
getErrorSeverity (TradingDisabled _) = SeverityCritical
getErrorSeverity (MarketClosed _) = SeverityMedium
getErrorSeverity (ParseError _ _) = SeverityHigh
getErrorSeverity (InvalidResponse _) = SeverityHigh
getErrorSeverity (MissingField _) = SeverityMedium
getErrorSeverity (DataLossWarning _ _ _) = SeverityLow
getErrorSeverity (ValidationError _) = SeverityMedium
getErrorSeverity (InvalidParameter _ _) = SeverityMedium
getErrorSeverity (UnsupportedOperation _) = SeverityHigh
getErrorSeverity (UnknownError _) = SeverityCritical
getErrorSeverity (InternalError _) = SeverityCritical


-- | Check if error is fatal (cannot be recovered).
isFatalError :: MT5Error -> Bool
isFatalError err = getErrorSeverity err == SeverityCritical


-- | Check if error is retryable (transient failure).
isRetryableError :: MT5Error -> Bool
isRetryableError (TimeoutError _ _) = True
isRetryableError (FileAccessError _) = True
isRetryableError (PythonProcessError _) = True
isRetryableError (BrokerError TRADE_RETCODE_TIMEOUT _) = True
isRetryableError (BrokerError TRADE_RETCODE_ERROR _) = True
isRetryableError (BrokerError TRADE_RETCODE_LOCKED _) = True
isRetryableError (BrokerError TRADE_RETCODE_TOO_MANY_REQUESTS _) = True
isRetryableError (BrokerError TRADE_RETCODE_REQUOTE _) = True
isRetryableError (BrokerError TRADE_RETCODE_CONNECTION _) = True
isRetryableError _ = False


-- | Execute operation with error handling wrapper.
--
-- Catches any exceptions and converts them to MT5Error.
handleMT5Error :: IO a -> IO (Either MT5Error a)
handleMT5Error action = do
  result <- try action
  case result of
    Right val -> return $ Right val
    Left (e :: SomeException) -> return $ Left $ UnknownError (T.pack $ show e)


-- | Execute operation with timeout.
--
-- Returns TimeoutError if operation exceeds timeout duration.
-- Uses System.Timeout.timeout from base library for proper timeout enforcement.
withTimeout :: Int  -- ^ Timeout in milliseconds
            -> Text -- ^ Operation name (for error message)
            -> IO a
            -> IO (Either MT5Error a)
withTimeout timeoutMs opName action = do
  -- Use System.Timeout.timeout (from base library)
  -- Converts milliseconds to microseconds (* 1000)
  result <- timeout (timeoutMs * 1000) $ handleMT5Error action
  case result of
    Nothing -> return $ Left $ TimeoutError opName timeoutMs
    Just (Left err) -> return $ Left err
    Just (Right val) -> return $ Right val


-- | Retry operation with exponential backoff.
--
-- Automatically retries transient failures with increasing delays.
retryWithBackoff :: RetryConfig -> IO (Either MT5Error a) -> IO (RetryResult a)
retryWithBackoff config action = go 1 (retryInitialDelay config)
  where
    go attempt currentDelay
      | attempt > retryMaxAttempts config = do
          result <- action
          case result of
            Left err -> return $ RetryFailed err attempt
            Right val -> return $ RetrySuccess val attempt
            
      | otherwise = do
          result <- action
          case result of
            Right val -> return $ RetrySuccess val attempt
            
            Left err ->
              if retryableErrors config err
                then do
                  -- Wait with exponential backoff
                  threadDelay (currentDelay * 1000) -- Convert ms to microseconds
                  let nextDelay = min (retryMaxDelay config) 
                                      (round $ fromIntegral currentDelay * retryBackoffFactor config)
                  go (attempt + 1) nextDelay
                else
                  return $ RetryGaveUp err attempt


-- | Apply recovery strategy to error.
applyRecovery :: RecoveryStrategy -> MT5Error -> IO (Either MT5Error a) -> IO (Either MT5Error a)
applyRecovery (RetryOperation config) _ action = do
  result <- retryWithBackoff config action
  case result of
    RetrySuccess val _ -> return $ Right val
    RetryFailed err _ -> return $ Left err
    RetryGaveUp err _ -> return $ Left err
    
applyRecovery PropagateError err _ = return $ Left err

applyRecovery LogAndContinue err action = do
  -- Simple logging to stdout 
  putStrLn $ "Warning: " ++ show err
  action
  
applyRecovery _ err _ = return $ Left err


-- | Safe execution with automatic error handling and retry.
--
-- Combines timeout, retry, and error handling in one convenient function.
--
-- ==== __Example__
--
-- >>> safeExecute defaultRetryConfig 5000 "accountInfo" accountInfo
-- Right (AccountInfo {...})
--
safeExecute :: RetryConfig  -- ^ Retry configuration
            -> Int          -- ^ Timeout in milliseconds
            -> Text         -- ^ Operation name
            -> IO a         -- ^ Action to execute
            -> IO (Either MT5Error a)
safeExecute config timeoutMs opName action = do
  result <- retryWithBackoff config $ withTimeout timeoutMs opName action
  case result of
    RetrySuccess val _ -> return $ Right val
    RetryFailed err _ -> return $ Left err
    RetryGaveUp err _ -> return $ Left err
