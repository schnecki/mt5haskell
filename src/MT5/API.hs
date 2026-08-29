{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module MT5.API
  ( -- * Symbol Groups
    SymbolGroup (..),
    -- * Authentication
    initialize
  , loginAccount
  , resetFileBridge
    -- * Daemon Priority
  , MT5Priority (..)
  , setMT5Priority
    -- * Account Information
  , accountInfo
    -- * Position Management
  , positionsGet
  , positionClose
  , positionClosePartial
  , positionModify
    -- * Order Management
  , ordersGet
  , cancelOrderPOST
  , cancelAllOrdersPOST
  , orderCheck
  , orderSend
    -- * Symbol Information
  , symbolInfo
  , symbolsGet
  , symbolSelect
    -- * Market Data
  , currentPriceGET
  , getCandleDataRange
  , getCandleDataFrom
  , getCandleDataRecent
  , copyTicksFrom
  , copyTicksRange
  , historyDealsRealizedForPosition
    -- * ExceptT Helper Functions
  , liftMaybe
  , eitherToExceptT
  , maybeToExceptT
    -- * Compatibility Wrappers (ExceptT → Either)
  , positionsGetEither
  , ordersGetEither
  , orderSendEither
  , accountInfoEither
  , symbolInfoEither
  , positionCloseEither
  , positionClosePartialEither
  , positionModifyEither
  , runWithDefault
  , runWithLogging
  ) where

import           Control.DeepSeq
import           Control.Exception           (SomeException, try)
import           Control.Monad               (replicateM)
import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON (..), Value, decode,
                                              encode, object, withObject,
                                              (.=), (.:))
import           Data.Aeson.Types            (parseMaybe)
import qualified Data.ByteString.Lazy        as BSL
import           Data.List                   (filter, find, isPrefixOf)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (NominalDiffTime, UTCTime,
                                              addUTCTime, getCurrentTime)
import           Data.Time.Format            (defaultTimeLocale, formatTime)
import           EasyLogger                  (logDebug, logDebugText, logInfo,
                                              logInfoText, logWarning,
                                              logWarningText)
import           GHC.Generics                (Generic)
import           System.IO.Unsafe            (unsafePerformIO)
import           System.Timeout              (timeout)


import           MT5.API.Internal            (sendRequestViaFile)
import           MT5.Communication.File      (getMT5FilesDirDefault,
                                              resetMT5Files)
import           MT5.Communication           (receive, send, unpickle', withMT5Lock)
import           MT5.Communication.PyProc    (MT5Priority (..),
                                              mt5CycleTimeoutMicros,
                                              setMT5Priority)
import           MT5.Communication.Request   (mkAccountInfoRequest,
                                              mkOrderSendRequest,
                                              mkOrdersGetRequest,
                                              mkPositionClosePartialRequest,
                                              mkPositionCloseRequest,
                                              mkPositionModifyRequest,
                                              mkPositionsGetRequest,
                                              mkSymbolInfoRequest)
import qualified MT5.Communication.Request   as Req
import           MT5.Communication.Response  (AccountInfoResponse (..),
                                              OrderInfoResponse (..),
                                              OrderSendResponse (..),
                                              OrdersGetResponse (..),
                                              PositionCloseResponse (..),
                                              PositionInfoResponse (..),
                                              PositionModifyResponse (..),
                                              PositionsGetResponse (..),
                                              SymbolInfoResponse (..))
import           MT5.Communication.Types     (ErrorResponse (..), Response (..),
                                              responseData)
import           MT5.Config                  (CommunicationChannel (..),
                                              Login (..), communicationChannel,
                                              getConfig,
                                              positionManagementChannel)
import           MT5.Data                    (AccountInfo (..),
                                              CurrentPrice (..),
                                              MqlTradeRequest (..),
                                              OrderSendResult (..),
                                              PositionReason (..),
                                              PositionType (..),
                                              SymbolInfo (..), TradeOrder (..),
                                              TradePosition (..),
                                              readOrderSendResult,
                                              readSymbolInfo)
import           MT5.Data.AccountInfo        (AccountMarginMode (..),
                                              AccountStopoutMode (..),
                                              AccountTradeMode (..))
import           MT5.Data.Candle             (MT5Candle (MT5Candle),
                                              MT5CandleData (MT5CandleData),
                                              mt5CandleTime, mt5Candles)
import           MT5.Data.Tick               (MT5Tick (..))
import           MT5.Data.DecimalNumber      (DecimalNumber (..),
                                              mkDecimalNumberFromDouble)
import           MT5.Data.Granularity        (MT5Granularity, granularitySeconds,
                                              toMT5TimeframeInt)
import           MT5.Data.OrderSendResult    (TradeRetcode (..), toTradeRetcode)
import           MT5.Data.OrderState         (OrderState (..))
import           MT5.Data.OrderType          (OrderType (..))
import           MT5.Data.OrderTypeFilling   (OrderTypeFilling (..))
import           MT5.Data.OrderTypeTime      (OrderTypeTime (..))
import           MT5.Data.SymbolInfo         (SymbolCalcMode (..),
                                              SymbolChartMode (..),
                                              SymbolOptionMode (..),
                                              SymbolOptionRight (..),
                                              SymbolOrders (..),
                                              SymbolSwapMode (..),
                                              SymbolTradeExecutionMode (..),
                                              SymbolTradeMode (..))
import           MT5.Data.TradeRequestAction (TradeRequestAction (..))
import           MT5.Error                   (MT5Error (..))
import           MT5.Util                    (mscToUTCTime, secondsToUTCTime)

type Symbol = String
type Ticket = Integer

-- ==============================================================================
-- EXCEPTT HELPER FUNCTIONS
-- ==============================================================================

-- | Convert 'Maybe' to 'ExceptT' with a custom error.
--
-- Use this helper when you have a 'Maybe' value that could fail and you want
-- to convert it to an 'ExceptT' computation with a specific error.
--
-- ==== __Examples__
--
-- >>> liftMaybe (ParseError "Response" "invalid json") (Just [1,2,3])
-- ExceptT (Right [1,2,3])
--
-- >>> liftMaybe (TimeoutError "operation" 5000) Nothing
-- ExceptT (Left (TimeoutError "operation" 5000))
--
-- @since 0.2.0.0
liftMaybe :: MT5Error -> Maybe a -> ExceptT MT5Error IO a
liftMaybe err Nothing  = throwError err
liftMaybe _ (Just x)   = return x

-- | Convert 'Either' to 'ExceptT'.
--
-- Use this helper to lift an 'Either' value into the 'ExceptT' monad transformer.
--
-- ==== __Examples__
--
-- >>> eitherToExceptT (Right 42)
-- ExceptT (Right 42)
--
-- >>> eitherToExceptT (Left (ParseError "field" "message"))
-- ExceptT (Left (ParseError "field" "message"))
--
-- @since 0.2.0.0
eitherToExceptT :: Either MT5Error a -> ExceptT MT5Error IO a
eitherToExceptT (Left err) = throwError err
eitherToExceptT (Right x)  = return x

-- | Convert 'Maybe' to 'ExceptT' with a custom error (alias for 'liftMaybe').
--
-- @since 0.2.0.0
maybeToExceptT :: MT5Error -> Maybe a -> ExceptT MT5Error IO a
maybeToExceptT = liftMaybe

-- | Lift a parsed-success result, classifying an error response from the MT5 EA
-- into a typed 'MT5Error' (e.g. 'MarketClosed', 'TradingDisabled', 'BrokerError')
-- BEFORE falling back to a generic 'ParseError'.
--
-- The MT5 EA emits two shapes of responses:
--
--   * Success: @{"success": true, ...payload...}@ — parses into the response's
--     dedicated @FromJSON@ instance.
--   * Failure: @{"success": false, "error_code": <int>, "error_message": "..."}@
--     — would fail to parse as the success shape because required fields are
--     missing, producing a generic 'ParseError'.  Callers (e.g. live-trading
--     retry loops) cannot tell from a 'ParseError' whether to retry or give up.
--
-- This helper inspects @responseSuccess@ first; on failure it decodes the body
-- as 'ErrorResponse' and constructs a structured error so 'isRetryableError'
-- correctly returns 'False' for non-retryable conditions like a closed market.
--
-- @since 0.2.0.0
liftResponseOrTypedError
  :: Text                    -- ^ Operation / response-type name (for diagnostics)
  -> Response                -- ^ Raw response from the MT5 EA
  -> Maybe a                 -- ^ Result of decoding @responseData@ as the success shape
  -> ExceptT MT5Error IO a
liftResponseOrTypedError respName response mResp
  | responseSuccess response = liftMaybe (ParseError respName (T.pack (show response))) mResp
  | otherwise =
      let raw = T.pack (show response)
          mErr = parseMaybe parseJSON (responseData response) :: Maybe ErrorResponse
      in case mErr of
           Just (ErrorResponse code msg) ->
             throwError (classifyErrorResponse respName code msg)
           Nothing ->
             -- success=false but no error_code/error_message; surface as InvalidResponse
             -- rather than ParseError so retries still bail out fast.
             throwError (InvalidResponse (respName <> ": success=false without error fields; raw=" <> raw))

-- | Map an MT5 EA @error_code@ to the most specific 'MT5Error' constructor.
-- Falls back to 'BrokerError' with the parsed 'TradeRetcode' (which may be
-- 'TRADE_RETCODE_UNKNOWN' for codes outside the standard retcode range).
classifyErrorResponse :: Text -> Int -> Text -> MT5Error
classifyErrorResponse respName code msg =
  case code of
    10018 -> MarketClosed     msg
    10017 -> TradingDisabled  msg
    10019 -> BrokerError (toTradeRetcode 10019) (respName <> ": " <> msg)
    _     ->
      let retcode = toTradeRetcode code
          context = respName <> ": " <> msg
      in BrokerError retcode context

-- ==============================================================================
-- COMPATIBILITY WRAPPERS (ExceptT → Either)
-- ==============================================================================

-- | Run 'ExceptT' computation and return 'IO (Either MT5Error a)' for backward compatibility.
--
-- Use this to convert ExceptT computations to Either style at application boundaries.
--
-- ==== __Examples__
--
-- >>> positions <- positionsGetEither
-- >>> case positions of
-- >>>   Left err -> putStrLn $ "Error: " ++ show err
-- >>>   Right ps -> print ps
--
-- @since 0.2.0.0
positionsGetEither :: IO (Either MT5Error [TradePosition])
positionsGetEither = runExceptT positionsGet

-- | Run ordersGet as 'IO (Either MT5Error [TradeOrder])' for backward compatibility.
--
-- @since 0.2.0.0
ordersGetEither :: Maybe Symbol -> Maybe Ticket -> IO (Either MT5Error [TradeOrder])
ordersGetEither mSym mTicket = runExceptT (ordersGet mSym mTicket)

-- | Run orderSend as 'IO (Either MT5Error OrderSendResult)' for backward compatibility.
--
-- @since 0.2.0.0
orderSendEither :: MqlTradeRequest -> IO (Either MT5Error OrderSendResult)
orderSendEither request = runExceptT (orderSend request)

-- | Run accountInfo as 'IO (Either MT5Error AccountInfo)' for backward compatibility.
--
-- @since 0.2.0.0
accountInfoEither :: IO (Either MT5Error AccountInfo)
accountInfoEither = runExceptT accountInfo

-- | Run symbolInfo as 'IO (Either MT5Error SymbolInfo)' for backward compatibility.
--
-- @since 0.2.0.0
symbolInfoEither :: Symbol -> IO (Either MT5Error SymbolInfo)
symbolInfoEither symbol = runExceptT (symbolInfo symbol)

-- | Run positionClose as 'IO (Either MT5Error Bool)' for backward compatibility.
--
-- @since 0.2.0.0
positionCloseEither :: Ticket -> IO (Either MT5Error Bool)
positionCloseEither ticket = runExceptT (positionClose ticket)

-- | Run positionClosePartial as 'IO (Either MT5Error Bool)' for backward compatibility.
--
-- @since 0.2.0.0
positionClosePartialEither :: Ticket -> Double -> IO (Either MT5Error Bool)
positionClosePartialEither ticket volume = runExceptT (positionClosePartial ticket volume)

-- | Run positionModify as 'IO (Either MT5Error Bool)' for backward compatibility.
--
-- @since 0.2.0.0
positionModifyEither :: Ticket -> Double -> Double -> IO (Either MT5Error Bool)
positionModifyEither ticket sl tp = runExceptT (positionModify ticket sl tp)

-- | Run ExceptT computation with default value on error.
--
-- Useful when you want to provide a fallback value instead of handling errors explicitly.
--
-- ==== __Examples__
--
-- >>> positions <- runWithDefault [] positionsGet
-- >>> print positions  -- Will print [] if error occurs
--
-- @since 0.2.0.0
runWithDefault :: a -> ExceptT e IO a -> IO a
runWithDefault def action = do
  result <- runExceptT action
  return $ either (const def) id result

-- | Run ExceptT computation and log errors, returning 'Maybe'.
--
-- Returns 'Nothing' on error after logging, 'Just value' on success.
--
-- @since 0.2.0.0
runWithLogging :: Show e => ExceptT e IO a -> IO (Maybe a)
runWithLogging action = do
  result <- runExceptT action
  case result of
    Left err -> do
      $(logWarning) $ "Error: " ++ show err
      return Nothing
    Right x -> return (Just x)

-- ==============================================================================
-- SYMBOL GROUPS
-- ==============================================================================

-- | Predefined groups for symbol retrieval.
data SymbolGroup
  = Forex                  -- ^ Forex group symbols
  | CFD                    -- ^ CFD group symbols
  | Indices                -- ^ Indices group symbols
  | Commodities            -- ^ Commodities group symbols
  | CustomSymbolGroup String -- ^ Custom symbol group
  deriving (Show, Eq)

-- | Convert 'SymbolGroup' to the string expected by the MT5 server.
symbolGroupToString :: SymbolGroup -> String
symbolGroupToString grp = case grp of
  Forex               -> "Forex"
  CFD                 -> "CFD"
  Indices             -> "Indices"
  Commodities         -> "Commodities"
  CustomSymbolGroup s -> s

-- | Initialize the MetaTrader 5 connection.
--
-- Sends the 'INITIALIZE' command to the Python server to set up the MT5 environment.
-- Returns 'Right ()' on success, or 'Left error' on failure.
--
-- Corresponds to MetaTrader5.initialize().
initialize :: IO (Either String ())
initialize = do
  -- Bound the send/receive so a stalled daemon during startup cannot block
  -- forever. A bare 'timeout' (not 'withMT5Lock') avoids re-entering the
  -- reconnect action while the connection is still being established.
  mRes <- timeout mt5CycleTimeoutMicros $ do
    send "INITIALIZE"
    unpickle' "Bool" <$> receive
  case mRes of
    Nothing    -> return $ Left "INITIALIZE timed out"
    Just True  -> return $ Right ()
    Just False -> Left <$> getError "failed to initialize to account #{}, error code: {}"

-- | Reset the file bridge by writing empty JSON to both request and response files.
--
-- Clears stale modification timestamps so the next request is not blocked waiting
-- for an outdated response.  Call this when repeated file-bridge requests fail
-- (e.g. all cancel attempts return Left False) to restore a clean communication
-- state before retrying.  Resolves file paths from the current global Config.
resetFileBridge :: IO ()
resetFileBridge = do
  config <- getConfig
  (reqPath, respPath) <- case communicationChannel config of
    FileBridgeCustom r s -> return (r, s)
    _ -> do
      filesDir <- getMT5FilesDirDefault
      return (filesDir ++ "/mt5_api_request.json", filesDir ++ "/mt5_api_response.json")
  resetMT5Files reqPath respPath

-- | Log in to a MetaTrader 5 account.
--
-- Sends the 'LOGIN' command with account credentials to the Python server.
-- Returns 'Right ()' on success, or 'Left error' on failure.
--
loginAccount :: Login           -- ^ Account login credentials (username and password)
             -> IO (Either String ())
loginAccount Login {..} = do
  -- Bound the login handshake; a bare 'timeout' avoids reconnect re-entry
  -- during startup (see 'initialize').
  mRes <- timeout mt5CycleTimeoutMicros $ do
    send "LOGIN"
    send account
    send password
    unpickle' "Bool" <$> receive
  case mRes of
    Nothing    -> return $ Left "LOGIN timed out"
    Just True  -> return $ Right ()
    Just False -> Left <$> getError "failed to connect to account #{}, error code: {}"


-- | Retrieve account information for the current session.
-- | Retrieve account information.
--
-- Uses 'ExceptT' for type-safe error handling with automatic error propagation.
--
-- **Default Routing**: Prefers PythonBridge (returns 28 complete fields) over FileBridge (14 fields = 50% data loss).
-- Config can override to FileBridge if explicitly needed, but PythonBridge strongly recommended for production.
--
-- **Errors**:
-- - 'TimeoutError': Request timed out after 5 seconds
-- - 'ParseError': Failed to parse response from MT5 EA
-- - 'PythonProcessError': Python bridge communication failed
--
-- ==== __Examples__
--
-- >>> result <- runExceptT accountInfo
-- >>> case result of
-- >>>   Left err -> putStrLn $ "Error: " ++ show err
-- >>>   Right info -> print info
--
-- Supports dual communication channels:
-- - FileBridge: Uses file-based communication with MT5RestAPIBridge.mq5 (faster but incomplete data)
-- - PythonBridge: Uses Python-based communication (complete data, recommended default)
--
-- Corresponds to MetaTrader5.account_info().
--
-- @since 0.2.0.0
accountInfo :: ExceptT MT5Error IO AccountInfo
accountInfo = do
  config <- liftIO getConfig
  case communicationChannel config of
    FileBridge           -> accountInfoViaFile
    FileBridgeCustom _ _ -> accountInfoViaFile
    PythonBridge         -> accountInfoViaPython

-- | Get account info via file-based communication
accountInfoViaFile :: ExceptT MT5Error IO AccountInfo
accountInfoViaFile = do
  liftIO $ $(logInfoText) "Fetching account info via FileBridge"
  let req = mkAccountInfoRequest

  -- Send request and wait for response (5 second timeout)
  mResponse <- liftIO $ sendRequestViaFile "account_info" req 5000
  response <- liftMaybe (TimeoutError "account_info" 5000) mResponse

  -- Parse the response data as AccountInfoResponse
  let mAccountInfo = decode (encode $ responseData response) :: Maybe AccountInfoResponse
  accResp <- liftResponseOrTypedError "AccountInfoResponse" response mAccountInfo
  
  liftIO $ $(logDebugText) "Successfully parsed account info response"
  return $ convertAccountInfoResponse accResp

-- | Convert AccountInfoResponse to AccountInfo
convertAccountInfoResponse :: AccountInfoResponse -> AccountInfo
convertAccountInfoResponse resp = AccountInfo
  { accInfoLogin              = accountInfoLogin resp
  , accInfoTrade_mode         = ACCOUNT_TRADE_MODE_DEMO  -- Default, not in response
  , accInfoLeverage           = accountInfoLeverage resp
  , accInfoLimit_orders       = 0  -- Not in file response
  , accInfoMargin_so_mode     = ACCOUNT_STOPOUT_MODE_PERCENT  -- Default
  , accInfoTrade_allowed      = accountInfoTradeAllowed resp
  , accInfoTrade_expert       = accountInfoTradeExpert resp
  , accInfoMargin_mode        = ACCOUNT_MARGIN_MODE_RETAIL_NETTING  -- Default
  , accInfoCurrency_digits    = 2  -- Default for most currencies
  , accInfoFifo_close         = False  -- Default
  , accInfoBalance            = accountInfoBalance resp
  , accInfoCredit             = 0.0  -- Not in file response
  , accInfoProfit             = accountInfoProfit resp
  , accInfoEquity             = accountInfoEquity resp
  , accInfoMargin             = accountInfoMargin resp
  , accInfoMargin_free        = accountInfoMarginFree resp
  , accInfoMargin_level       = accountInfoMarginLevel resp
  , accInfoMargin_so_call     = 0.0  -- Not in file response
  , accInfoMargin_so_so       = 0.0  -- Not in file response
  , accInfoMargin_initial     = 0.0  -- Not in file response
  , accInfoMargin_maintenance = 0.0  -- Not in file response
  , accInfoAssets             = 0.0  -- Not in file response
  , accInfoLiabilities        = 0.0  -- Not in file response
  , accInfoCommission_blocked = 0.0  -- Not in file response
  , accInfoName               = T.unpack $ accountInfoName resp
  , accInfoServer             = T.unpack $ accountInfoServer resp
  , accInfoCurrency           = T.unpack $ accountInfoCurrency resp
  , accInfoCompany            = ""  -- Not in file response
  }

-- | Get account info via Python bridge (legacy compatibility)
accountInfoViaPython :: ExceptT MT5Error IO AccountInfo
accountInfoViaPython = do
  liftIO $ $(logInfoText) "Fetching account info via PythonBridge"

  -- Wrap in try block to catch any exceptions (Option B approach)
  result <- liftIO $ withMT5Lock $ try $ do
    send "ACCOUNT_INFO"
    AccountInfo
      <$> (unpickle' "Int" <$> receive)
      <*> (toEnum . unpickle' "Int" <$> receive)
      <*> (unpickle' "Int" <$> receive)
      <*> (unpickle' "Int" <$> receive)
      <*> (toEnum . unpickle' "Int" <$> receive)
      <*> (unpickle' "Bool" <$> receive)
      <*> (unpickle' "Bool" <$> receive)
      <*> (toEnum . unpickle' "Int" <$> receive)
      <*> (unpickle' "Int" <$> receive)
      <*> (unpickle' "Bool" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "Double" <$> receive)
      <*> (unpickle' "String" <$> receive)
      <*> (unpickle' "String" <$> receive)
      <*> (unpickle' "String" <$> receive)
      <*> (unpickle' "String" <$> receive)

  case result of
    Left (e :: SomeException) -> do
      liftIO $ $(logWarning) $ "Python bridge error: " ++ show e
      throwError $ PythonProcessError (T.pack $ show e)
    Right info -> do
      liftIO $ $(logDebugText) "Successfully fetched account info via Python"
      return info

-- | Request the last error message from the Python server.
--
-- Sends the 'ERROR' command with a format string and receives the error message.
--
-- Corresponds to MetaTrader5.last_error().
getError :: String             -- ^ Format string for the error request
         -> IO String
getError formatString = withMT5Lock $ do
  send "ERROR"
  send formatString
  unpickle' "String" <$> receive


-- | Retrieve all open positions for the current account.
--
-- Uses 'ExceptT' for type-safe error handling with automatic error propagation.
--
-- **Default Routing**: Prefers PythonBridge (returns 19 complete fields) over FileBridge (12 fields = 37% data loss).
-- Config can override to FileBridge if explicitly needed, but PythonBridge strongly recommended for production.
--
-- **Errors**:
-- - 'TimeoutError': Request timed out after 5 seconds
-- - 'ParseError': Failed to parse response from MT5 EA
-- - 'BrokerError': MT5 returned error (e.g., connection lost)
-- - 'PythonProcessError': Python bridge communication failed
--
-- ==== __Examples__
--
-- >>> -- Direct usage with runExceptT
-- >>> result <- runExceptT positionsGet
-- >>> case result of
-- >>>   Left err -> putStrLn $ "Error: " ++ show err
-- >>>   Right positions -> print positions
--
-- >>> -- Composition with automatic error propagation
-- >>> tradingOp = do
-- >>>   positions <- positionsGet
-- >>>   orders <- ordersGet Nothing Nothing
-- >>>   return (length positions, length orders)
-- >>> result <- runExceptT tradingOp
--
-- >>> -- Using compatibility wrapper for Either
-- >>> result <- positionsGetEither
--
-- Supports dual communication channels:
-- - FileBridge: Uses file-based communication with MT5RestAPIBridge.mq5 (faster but incomplete data)
-- - PythonBridge: Uses Python-based communication (complete data, recommended default)
--
-- Corresponds to MetaTrader5.positions_get().
--
-- @since 0.2.0.0
positionsGet :: ExceptT MT5Error IO [TradePosition]
positionsGet = do
  config <- liftIO getConfig
  case positionManagementChannel config of
    FileBridge           -> positionsGetViaFile Nothing
    FileBridgeCustom _ _ -> positionsGetViaFile Nothing
    PythonBridge         -> positionsGetViaPython

-- | Get positions via file-based communication
positionsGetViaFile :: Maybe Symbol -> ExceptT MT5Error IO [TradePosition]
positionsGetViaFile mSymbol = do
  liftIO $ $(logInfo) $ "Fetching positions via FileBridge" ++ maybe "" (\s -> " for symbol: " ++ s) mSymbol
  let req = mkPositionsGetRequest (fmap T.pack mSymbol)

  -- Send request and wait for response (5 second timeout)
  mResponse <- liftIO $ sendRequestViaFile "positions_get" req 5000
  response <- liftMaybe (TimeoutError "positions_get" 5000) mResponse

  -- Parse the response data as PositionsGetResponse
  let mPositionsResp = decode (encode $ responseData response) :: Maybe PositionsGetResponse
  posResp <- liftResponseOrTypedError "PositionsGetResponse" response mPositionsResp

  if positionsGetSuccess posResp
    then do
      let positions = map convertPositionInfoResponse (positionsGetPositions posResp)
      liftIO $ $(logDebug) $ "Successfully parsed " ++ show (length positions) ++ " positions"
      return positions
    else
      throwError $ BrokerError TRADE_RETCODE_ERROR (T.pack "Positions get failed")

-- | Convert PositionInfoResponse to TradePosition
convertPositionInfoResponse :: PositionInfoResponse -> TradePosition
convertPositionInfoResponse resp = TradePosition
  { trPosTicket          = positionTicket resp
  , trPosTime            = secondsToUTCTime 0  -- Not available in file response
  , trPosTime_msc        = mscToUTCTime 0  -- Not available in file response
  , trPosTime_update     = secondsToUTCTime 0  -- Not available in file response
  , trPosTime_update_msc = mscToUTCTime 0  -- Not available in file response
  , trPosType            = toEnum (positionType resp)
  , trPosMagic           = positionMagic resp
  , trPosIdentifier      = 0  -- Not available in file response
  , trPosReason          = POSITION_REASON_CLIENT  -- Default
  , trPosVolume          = positionVolume resp
  , trPosPriceOpen       = positionPriceOpen resp
  , trPosSl              = positionSl resp
  , trPosTp              = positionTp resp
  , trPosPrice_current   = positionPriceCurrent resp
  , trPosSwap            = positionSwap resp
  , trPosProfit          = positionProfit resp
  , trPosSymbol          = T.unpack $ positionSymbol resp
  , trPosComment         = T.unpack $ positionComment resp
  , trPosExternal_id     = ""  -- Not available in file response
  }

-- | Get positions via Python bridge (legacy compatibility)
positionsGetViaPython :: ExceptT MT5Error IO [TradePosition]
positionsGetViaPython = do
  liftIO $ $(logInfoText) "Fetching positions via PythonBridge"

  -- Wrap in try block to catch any exceptions (Option B approach)
  result <- liftIO $ withMT5Lock $ try $ do
    send "POSITIONS_GET"
    len <- unpickle' "Int" <$> receive
    replicateM len
      $ TradePosition
          <$> (unpickle' "Int" <$> receive)
          <*> (secondsToUTCTime . unpickle' "Integer" <$> receive)
          <*> (mscToUTCTime . unpickle' "Integer" <$> receive)
          <*> (secondsToUTCTime . unpickle' "Integer" <$> receive)
          <*> (mscToUTCTime . unpickle' "Integer" <$> receive)
          <*> (toEnum . unpickle' "Int" <$> receive)
          <*> (unpickle' "Int" <$> receive)
          <*> (unpickle' "Int" <$> receive)
          <*> (toEnum . unpickle' "Int" <$> receive)
          <*> (either (const (DecimalNumber Nothing 0.0)) id . mkDecimalNumberFromDouble . unpickle' "Double" <$> receive)
          <*> (unpickle' "Double" <$> receive)
          <*> (unpickle' "Double" <$> receive)
          <*> (unpickle' "Double" <$> receive)
          <*> (unpickle' "Double" <$> receive)
          <*> (unpickle' "Double" <$> receive)
          <*> (unpickle' "Double" <$> receive)
          <*> (unpickle' "String" <$> receive)
          <*> (unpickle' "String" <$> receive)
          <*> (unpickle' "String" <$> receive)

  case result of
    Left (e :: SomeException) -> do
      liftIO $ $(logWarning) $ "Python bridge error: " ++ show e
      throwError $ PythonProcessError (T.pack $ show e)
    Right positions -> do
      liftIO $ $(logDebug) $ "Successfully fetched " ++ show (length positions) ++ " positions via Python"
      return positions

-- | Close a position completely.
--
-- Uses 'ExceptT' for type-safe error handling with automatic error propagation.
--
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for reliability).
--
-- For PythonBridge: Closes position by sending opposite order with position parameter set.
-- For FileBridge: Uses dedicated position_close EA action.
--
-- **Errors**:
-- - 'TimeoutError': Request timed out after 5 seconds
-- - 'ParseError': Failed to parse response from MT5 EA
-- - 'ValidationError': Invalid ticket or position not found
-- - 'BrokerError': MT5 broker returned error
--
-- ==== __Examples__
--
-- >>> result <- runExceptT $ positionClose 12345
-- >>> case result of
-- >>>   Left err -> putStrLn $ "Error: " ++ show err
-- >>>   Right success -> print success
--
-- Corresponds to position_close EA action (FileBridge) or opposite order technique (PythonBridge).
--
-- @since 0.2.0.0
positionClose :: Ticket -> ExceptT MT5Error IO Bool
positionClose ticket = do
  config <- liftIO getConfig
  case positionManagementChannel config of
    FileBridge           -> positionCloseViaFile ticket
    FileBridgeCustom _ _ -> positionCloseViaFile ticket
    PythonBridge         -> positionCloseViaPython ticket

-- | Close a position via file-based communication
positionCloseViaFile :: Ticket -> ExceptT MT5Error IO Bool
positionCloseViaFile ticket = do
  let eitherReq = mkPositionCloseRequest (fromIntegral ticket)
  req <- eitherToExceptT $ case eitherReq of
    Left err -> Left $ ValidationError $ T.pack $ "Invalid position close request: " ++ show err
    Right r -> Right r
  
  mResponse <- liftIO $ sendRequestViaFile "position_close" req 5000
  response <- liftMaybe (TimeoutError "position_close" 5000) mResponse
  
  let mResp = decode (encode $ responseData response) :: Maybe PositionCloseResponse
  resp <- liftResponseOrTypedError "PositionCloseResponse" response mResp
  return $ positionCloseSuccess resp

-- | Close a position via Python bridge
--
-- Closes position by sending an opposite order with the position parameter set.
-- This is the standard MT5 method when mt5.Close() is not available.
positionCloseViaPython :: Ticket -> ExceptT MT5Error IO Bool
positionCloseViaPython ticket = do
  -- First get the position to know its details
  positions <- positionsGet
  case filter (\p -> trPosTicket p == ticket) positions of
    [] -> throwError $ ValidationError $ T.pack $ "Position not found: " ++ show ticket
    (pos:_) -> do
      -- Create opposite order to close the position
      let oppositeType = case trPosType pos of
            POSITION_TYPE_BUY  -> ORDER_TYPE_SELL
            POSITION_TYPE_SELL -> ORDER_TYPE_BUY

      let closeRequest = MqlTradeRequest
            { trReqAction      = TRADE_ACTION_DEAL
            , trReqMagic       = 0
            , trReqOrder       = 0
            , trReqSymbol      = trPosSymbol pos
            , trReqVolume      = trPosVolume pos
            , trReqPrice       = DecimalNumber Nothing 0.0  -- Market price
            , trReqStoplimit   = DecimalNumber Nothing 0.0
            , trReqSl          = DecimalNumber Nothing 0.0
            , trReqTp          = DecimalNumber Nothing 0.0
            , trReqDeviation   = 10
            , trReqType        = oppositeType
            , trReqTypeFilling = ORDER_FILLING_FOK
            , trReqTypeTime    = ORDER_TIME_GTC
            , trReqExpiration  = 0
            , trReqComment     = "Close position " ++ show ticket
            , trReqPosition    = ticket  -- CRITICAL: This closes the specific position
            , trReqPositionBy  = 0
            }

      result <- orderSendViaPython closeRequest
      return $ (== TRADE_RETCODE_DONE) . ordSendRetcode $ result

-- | Close a position partially (reduce volume).
--
-- Uses 'ExceptT' for type-safe error handling with automatic error propagation.
--
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for reliability).
--
-- For PythonBridge: Closes partial volume by sending opposite order with smaller volume.
-- For FileBridge: Uses dedicated position_close_partial EA action.
--
-- **Errors**:
-- - 'TimeoutError': Request timed out after 5 seconds
-- - 'ParseError': Failed to parse response from MT5 EA
-- - 'ValidationError': Invalid ticket, volume, or position not found
--
-- ==== __Examples__
--
-- >>> result <- runExceptT $ positionClosePartial 12345 0.5
-- >>> case result of
-- >>>   Left err -> putStrLn $ "Error: " ++ show err
-- >>>   Right success -> print success
--
-- Corresponds to position_close_partial EA action.
--
-- @since 0.2.0.0
positionClosePartial :: Ticket -> Double -> ExceptT MT5Error IO Bool
positionClosePartial ticket volume = do
  config <- liftIO getConfig
  case positionManagementChannel config of
    FileBridge           -> positionClosePartialViaFile ticket volume
    FileBridgeCustom _ _ -> positionClosePartialViaFile ticket volume
    PythonBridge         -> positionClosePartialViaPython ticket volume

-- | Close position partially via file-based communication
positionClosePartialViaFile :: Ticket -> Double -> ExceptT MT5Error IO Bool
positionClosePartialViaFile ticket volume = do
  -- Convert Double to DecimalNumber
  let volResult = mkDecimalNumberFromDouble volume
  vol <- eitherToExceptT $ case volResult of
    Left _ -> Left $ ValidationError $ T.pack $ "Invalid volume: " ++ show volume
    Right v -> Right v
  
  let eitherReq = mkPositionClosePartialRequest (fromIntegral ticket) vol
  req <- eitherToExceptT $ case eitherReq of
    Left err -> Left $ ValidationError $ T.pack $ "Invalid position close partial request: " ++ show err
    Right r -> Right r
  
  mResponse <- liftIO $ sendRequestViaFile "position_close_partial" req 5000
  response <- liftMaybe (TimeoutError "position_close_partial" 5000) mResponse
  
  let mResp = decode (encode $ responseData response) :: Maybe PositionCloseResponse
  resp <- liftResponseOrTypedError "PositionCloseResponse" response mResp
  return $ positionCloseSuccess resp

-- | Modify a position's stop-loss and take-profit levels.
--
-- Uses 'ExceptT' for type-safe error handling with automatic error propagation.
--
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for reliability).
--
-- For PythonBridge: Modifies position SL/TP using TRADE_ACTION_SLTP.
-- For FileBridge: Uses dedicated position_modify EA action.
--
-- **Errors**:
-- - 'TimeoutError': Request timed out after 5 seconds
-- - 'ParseError': Failed to parse response from MT5 EA
-- - 'ValidationError': Invalid ticket or position not found
--
-- ==== __Examples__
--
-- >>> result <- runExceptT $ positionModify 12345 1.2000 1.2100
-- >>> case result of
-- >>>   Left err -> putStrLn $ "Error: " ++ show err
-- >>>   Right success -> print success
--
-- Corresponds to position_modify EA action.
--
-- @since 0.2.0.0
positionModify :: Ticket -> Double -> Double -> ExceptT MT5Error IO Bool
positionModify ticket sl tp = do
  config <- liftIO getConfig
  case positionManagementChannel config of
    FileBridge           -> positionModifyViaFile ticket sl tp
    FileBridgeCustom _ _ -> positionModifyViaFile ticket sl tp
    PythonBridge         -> positionModifyViaPython ticket sl tp

-- | Modify position via file-based communication
positionModifyViaFile :: Ticket -> Double -> Double -> ExceptT MT5Error IO Bool
positionModifyViaFile ticket sl tp = do
  let eitherReq = mkPositionModifyRequest (fromIntegral ticket) sl tp
  req <- eitherToExceptT $ case eitherReq of
    Left err -> Left $ ValidationError $ T.pack $ "Invalid position modify request: " ++ show err
    Right r -> Right r
  
  mResponse <- liftIO $ sendRequestViaFile "position_modify" req 5000
  response <- liftMaybe (TimeoutError "position_modify" 5000) mResponse
  
  let mResp = decode (encode $ responseData response) :: Maybe PositionModifyResponse
  resp <- liftResponseOrTypedError "PositionModifyResponse" response mResp
  return $ positionModifySuccess resp

-- | Close partial position via Python bridge
positionClosePartialViaPython :: Ticket -> Double -> ExceptT MT5Error IO Bool
positionClosePartialViaPython ticket volume = do
  -- Convert Double to DecimalNumber
  let volResult = mkDecimalNumberFromDouble volume
  vol <- eitherToExceptT $ case volResult of
    Left _ -> Left $ ValidationError $ T.pack $ "Invalid volume: " ++ show volume
    Right v -> Right v
  
  -- First get the position to know its details
  positions <- positionsGet
  case filter (\p -> trPosTicket p == ticket) positions of
    [] -> throwError $ ValidationError $ T.pack $ "Position not found: " ++ show ticket
    (pos:_) -> do
      -- Create opposite order with partial volume to close
      let oppositeType = case trPosType pos of
            POSITION_TYPE_BUY  -> ORDER_TYPE_SELL
            POSITION_TYPE_SELL -> ORDER_TYPE_BUY

      let closeRequest = MqlTradeRequest
            { trReqAction      = TRADE_ACTION_DEAL
            , trReqMagic       = 0
            , trReqOrder       = 0
            , trReqSymbol      = trPosSymbol pos
            , trReqVolume      = vol  -- Partial volume (now DecimalNumber type)
            , trReqPrice       = DecimalNumber Nothing 0.0  -- Market price
            , trReqStoplimit   = DecimalNumber Nothing 0.0
            , trReqSl          = DecimalNumber Nothing 0.0
            , trReqTp          = DecimalNumber Nothing 0.0
            , trReqDeviation   = 10
            , trReqType        = oppositeType
            , trReqTypeFilling = ORDER_FILLING_FOK
            , trReqTypeTime    = ORDER_TIME_GTC
            , trReqExpiration  = 0
            , trReqComment     = "Close partial " ++ show ticket
            , trReqPosition    = ticket  -- CRITICAL: This closes the specific position
            , trReqPositionBy  = 0
            }

      result <- orderSendViaPython closeRequest
      return $ (TRADE_RETCODE_DONE ==) . ordSendRetcode $ result

-- | Modify position via Python bridge
positionModifyViaPython :: Ticket -> Double -> Double -> ExceptT MT5Error IO Bool
positionModifyViaPython ticket sl tp = do
  -- Get position to know its symbol
  positions <- positionsGet
  case filter (\p -> trPosTicket p == ticket) positions of
    [] -> throwError $ ValidationError $ T.pack $ "Position not found: " ++ show ticket
    (pos:_) -> do
      let modifyRequest = MqlTradeRequest
            { trReqAction      = TRADE_ACTION_SLTP
            , trReqMagic       = 0
            , trReqOrder       = 0
            , trReqSymbol      = trPosSymbol pos
            , trReqVolume      = DecimalNumber Nothing 0.0  -- Not needed for SLTP
            , trReqPrice       = DecimalNumber Nothing 0.0  -- Not needed for SLTP
            , trReqStoplimit   = DecimalNumber Nothing 0.0
            , trReqSl          = DecimalNumber Nothing sl   -- New stop loss
            , trReqTp          = DecimalNumber Nothing tp   -- New take profit
            , trReqDeviation   = 0
            , trReqType        = ORDER_TYPE_BUY  -- Doesn't matter for SLTP
            , trReqTypeFilling = ORDER_FILLING_FOK
            , trReqTypeTime    = ORDER_TIME_GTC
            , trReqExpiration  = 0
            , trReqComment     = "Modify SL/TP " ++ show ticket
            , trReqPosition    = ticket  -- CRITICAL: Position to modify
            , trReqPositionBy  = 0
            }

      result <- orderSendViaPython modifyRequest
      return $ (== TRADE_RETCODE_DONE) . ordSendRetcode $ result

-- | Get active orders with the ability to filter by symbol or ticket.
--
-- Uses 'ExceptT' for type-safe error handling with automatic error propagation.
--
-- **Default Routing**: Prefers PythonBridge (returns 17 complete fields) over FileBridge (10 fields = 41% data loss).
-- Config can override to FileBridge if explicitly needed, but PythonBridge strongly recommended for production.
--
-- **Note**: Ticket filtering only works with PythonBridge (FileBridge ignores ticket parameter).
--
-- **Errors**:
-- - 'TimeoutError': Request timed out after 5 seconds
-- - 'ParseError': Failed to parse response from MT5 EA
-- - 'BrokerError': MT5 returned error (e.g., connection lost)
-- - 'PythonProcessError': Python bridge communication failed
--
-- ==== __Examples__
--
-- >>> -- Get all orders
-- >>> result <- runExceptT $ ordersGet Nothing Nothing
-- >>> case result of
-- >>>   Left err -> putStrLn $ "Error: " ++ show err
-- >>>   Right orders -> print orders
--
-- >>> -- Get orders for specific symbol
-- >>> result <- runExceptT $ ordersGet (Just "EURUSD") Nothing
--
-- >>> -- Get specific order by ticket (PythonBridge only)
-- >>> result <- runExceptT $ ordersGet Nothing (Just 12345)
--
-- >>> -- Composition with automatic error propagation
-- >>> tradingOp = do
-- >>>   orders <- ordersGet Nothing Nothing
-- >>>   positions <- positionsGet
-- >>>   return (length orders, length positions)
-- >>> result <- runExceptT tradingOp
--
-- >>> -- Using compatibility wrapper for Either
-- >>> result <- ordersGetEither Nothing Nothing
--
-- Supports dual communication channels:
-- - FileBridge: Uses file-based communication with MT5RestAPIBridge.mq5 (faster but incomplete data, symbol filter only)
-- - PythonBridge: Uses Python-based communication (complete data + ticket filter, recommended default)
--
-- Corresponds to MetaTrader5.orders_get().
--
-- @since 0.2.0.0
--
ordersGet :: Maybe Symbol      -- ^ Optional symbol filter (e.g., Just "EURUSD")
          -> Maybe Ticket      -- ^ Optional ticket filter (e.g., Just 12345) - PythonBridge only
          -> ExceptT MT5Error IO [TradeOrder]
ordersGet mInstr mTicket = do
  config <- liftIO getConfig
  case communicationChannel config of
    FileBridge           -> ordersGetViaFile mInstr mTicket
    FileBridgeCustom _ _ -> ordersGetViaFile mInstr mTicket
    PythonBridge         -> ordersGetViaPython mInstr mTicket

-- | Retrieve orders using file bridge (Note: ticket filter not supported by EA)
ordersGetViaFile :: Maybe Symbol -> Maybe Ticket -> ExceptT MT5Error IO [TradeOrder]
ordersGetViaFile mSymbol _mTicket = do
  liftIO $ $(logInfo) $ "Fetching orders via FileBridge" ++ maybe "" (\s -> " for symbol: " ++ s) mSymbol
  liftIO $ $(logWarningText) "Note: FileBridge does not support ticket filtering"
  let req = mkOrdersGetRequest (fmap T.pack mSymbol)
  mResponse <- liftIO $ sendRequestViaFile "orders_get" req 5000
  response <- liftMaybe (TimeoutError "orders_get" 5000) mResponse

  let mOrders = decode (encode $ responseData response) :: Maybe OrdersGetResponse
  resp <- liftResponseOrTypedError "OrdersGetResponse" response mOrders

  if ordersGetSuccess resp
    then do
      let orders = map convertOrderInfoResponse (ordersGetOrders resp)
      liftIO $ $(logDebug) $ "Successfully parsed " ++ show (length orders) ++ " orders"
      return orders
    else
      throwError $ BrokerError TRADE_RETCODE_ERROR (T.pack "Orders get failed")

-- | Retrieve orders using Python bridge (original implementation)
ordersGetViaPython :: Maybe Symbol -> Maybe Ticket -> ExceptT MT5Error IO [TradeOrder]
ordersGetViaPython mInstr mTicket = do
  liftIO $ $(logInfo) $ "Fetching orders via PythonBridge" ++ maybe "" (\s -> " for symbol: " ++ s) mInstr

  -- Wrap in try block to catch any exceptions (Option B approach)
  result <- liftIO $ withMT5Lock $ try $ do
    case (mInstr, mTicket) of
      (Just instr, Nothing) -> do
        send "ORDERS_GET_SYMBOL"
        send instr
      (_, Just ticket) -> do
        send "ORDERS_GET_TICKET"
        send (show ticket)
      _ -> do
        send "ORDERS_GET"
    len <- unpickle' "Int" <$> receive
    replicateM len
          $ TradeOrder
              <$> (unpickle' "Int" <$> receive)
              <*> (secondsToUTCTime . unpickle' "Integer" <$> receive)
              <*> (mscToUTCTime . unpickle' "Integer" <$> receive)
              <*> (unpickle' "Int" <$> receive)
              <*> (toEnum . unpickle' "Int" <$> receive)
              <*> (unpickle' "Integer" <$> receive)
              <*> (unpickle' "Int" <$> receive)
              <*> (toEnum . unpickle' "Int" <$> receive)
              <*> (unpickle' "Int" <$> receive)
              <*> (either (const (DecimalNumber Nothing 0.0)) id . mkDecimalNumberFromDouble . unpickle' "Double" <$> receive)
              <*> (unpickle' "Double" <$> receive)
              <*> (unpickle' "Double" <$> receive)
              <*> (unpickle' "Double" <$> receive)
              <*> (unpickle' "Double" <$> receive)
              <*> (unpickle' "String" <$> receive)
              <*> (unpickle' "String" <$> receive)
              <*> (unpickle' "String" <$> receive)

  case result of
    Left (e :: SomeException) -> do
      liftIO $ $(logWarning) $ "Python bridge error: " ++ show e
      throwError $ PythonProcessError (T.pack $ show e)
    Right orders -> do
      liftIO $ $(logDebug) $ "Successfully fetched " ++ show (length orders) ++ " orders via Python"
      return orders

-- | Convert OrderInfoResponse (10 fields) to TradeOrder (17 fields)
convertOrderInfoResponse :: OrderInfoResponse -> TradeOrder
convertOrderInfoResponse resp =
  let now = unsafePerformIO getCurrentTime
  in TradeOrder
    { tradeOrderTicket          = orderTicket resp
    , tradeOrderTime_setup      = now  -- Not in EA response
    , tradeOrderTime_setup_msc  = now  -- Not in EA response
    , tradeOrderTime_expiration = 0    -- Not in EA response
    , tradeOrderType            = toEnum (orderType resp)
    , tradeOrderType_time       = 0    -- Not in EA response
    , tradeOrderType_filling    = 0    -- Not in EA response
    , tradeOrderState           = ORDER_STATE_STARTED  -- Default (not in EA response)
    , tradeOrderMagic           = orderMagic resp
    , tradeOrderVolume_current  = orderVolume resp
    , tradeOrderPrice_open      = orderPriceOpen resp
    , tradeOrderSl              = orderSl resp
    , tradeOrderTp              = orderTp resp
    , tradeOrderPrice_current   = orderPriceCurrent resp
    , tradeOrderSymbol          = T.unpack $ orderSymbol resp
    , tradeOrderComment         = T.unpack $ orderComment resp
    , tradeOrderExternal_id     = ""   -- Not in EA response
    }


-- | Retrieve all symbols in a group.
--
symbolsGet :: Maybe SymbolGroup       -- ^ Predefined or custom symbol group
           -> IO [SymbolInfo]
symbolsGet mGroup = withMT5Lock $
  case mGroup of
    Just group -> do
      send "SYMBOLS_GET_GROUP"
      send $ symbolGroupToString group
      len <- unpickle' "Int" <$> receive
      replicateM len readSymbolInfo
    Nothing -> do
      send "SYMBOLS_GET"
      len <- unpickle' "Int" <$> receive
      replicateM len readSymbolInfo


-- | Retrieve information for a specific symbol.
--
-- Uses 'ExceptT' for type-safe error handling with automatic error propagation.
--
-- **CRITICAL - Default Routing**: ALWAYS use PythonBridge in production!
-- FileBridge returns only 7 of 104 fields (93% data loss) which may cause order rejections.
-- Missing: contract size, tick value, volume limits, swap rates, margin requirements, ALL options data.
--
-- **Default Routing**: Prefers PythonBridge (returns 104 complete fields) over FileBridge (7 fields = 93% data loss).
-- Config can override to FileBridge but this is STRONGLY DISCOURAGED for production use.
--
-- **Errors**:
-- - 'TimeoutError': Request timed out after 5 seconds
-- - 'ParseError': Failed to parse response from MT5 EA
-- - 'ValidationError': Invalid symbol name
-- - 'PythonProcessError': Python bridge communication failed
--
-- ==== __Examples__
--
-- >>> result <- runExceptT $ symbolInfo "EURUSD"
-- >>> case result of
-- >>>   Left err -> putStrLn $ "Error: " ++ show err
-- >>>   Right info -> print info
--
-- Supports dual communication channels:
-- - FileBridge: Uses file-based communication with MT5RestAPIBridge.mq5 (INCOMPLETE - only 7 basic fields)
-- - PythonBridge: Uses Python-based communication (COMPLETE - all 104 fields, REQUIRED for production)
--
-- @since 0.2.0.0
symbolInfo :: Symbol            -- ^ Symbol name to query (e.g., "EURUSD")
           -> ExceptT MT5Error IO SymbolInfo
symbolInfo symbol = do
  config <- liftIO getConfig
  case communicationChannel config of
    FileBridge           -> symbolInfoViaFile symbol
    FileBridgeCustom _ _ -> symbolInfoViaFile symbol
    PythonBridge         -> symbolInfoViaPython symbol

-- | Retrieve symbol information using file bridge
symbolInfoViaFile :: Symbol -> ExceptT MT5Error IO SymbolInfo
symbolInfoViaFile symbol = do
  liftIO $ $(logInfo) $ "Fetching symbol info via FileBridge for: " ++ symbol
  let reqResult = mkSymbolInfoRequest (T.pack symbol)
  req <- eitherToExceptT $ case reqResult of
    Left err -> Left $ ValidationError (T.pack $ "Invalid symbol request: " ++ show err)
    Right r -> Right r
  
  mResponse <- liftIO $ sendRequestViaFile "symbol_info" req 5000
  response <- liftMaybe (TimeoutError (T.pack $ "symbol_info:" ++ symbol) 5000) mResponse
  
  let mSymbolInfo = decode (encode $ responseData response) :: Maybe SymbolInfoResponse
  resp <- liftResponseOrTypedError "SymbolInfoResponse" response mSymbolInfo
  
  liftIO $ $(logDebug) $ "Successfully parsed symbol info for: " ++ symbol
  return $ convertSymbolInfoResponse resp

-- | Retrieve symbol information using Python bridge (original implementation)
symbolInfoViaPython :: Symbol -> ExceptT MT5Error IO SymbolInfo
symbolInfoViaPython symbol = do
  liftIO $ $(logInfo) $ "Fetching symbol info via PythonBridge for: " ++ symbol

  -- Wrap in try block to catch any exceptions (Option B approach)
  result <- liftIO $ withMT5Lock $ try $ do
    send "SYMBOL_INFO"
    send symbol
    readSymbolInfo

  case result of
    Left (e :: SomeException) -> do
      liftIO $ $(logWarning) $ "Python bridge error: " ++ show e
      throwError $ PythonProcessError (T.pack $ show e)
    Right info -> do
      liftIO $ $(logDebug) $ "Successfully fetched symbol info via Python for: " ++ symbol
      return info

-- | Convert SymbolInfoResponse (7 fields) to SymbolInfo (98+ fields with defaults)
convertSymbolInfoResponse :: SymbolInfoResponse -> SymbolInfo
convertSymbolInfoResponse resp =
  let now = unsafePerformIO getCurrentTime
  in SymbolInfo
  { symInfoCustom                  = False
  , symInfoChartMode               = SYMBOL_CHART_MODE_BID
  , symInfoSelect                  = True
  , symInfoVisible                 = False
  , symInfoSessionDeals            = 0
  , symInfoSessionBuyOrders        = SYMBOL_ORDERS_GTC
  , symInfoSessionSellOrders       = SYMBOL_ORDERS_GTC
  , symInfoVolume                  = 0
  , symInfoVolumehigh              = 0
  , symInfoVolumelow               = 0
  , symInfoTime                    = 0
  , symInfoDigits                  = symbolInfoDigits resp
  , symInfoSpread                  = symbolInfoSpread resp
  , symInfoSpreadFloat             = False
  , symInfoTicksBookdepth          = 0
  , symInfoTradeCalcMode           = SYMBOL_CALC_MODE_FOREX
  , symInfoTradeMode               = SYMBOL_TRADE_MODE_FULL
  , symInfoStartTime               = now
  , symInfoExpirationTime          = now
  , symInfoTradeStopsLevel         = 0
  , symInfoTradeFreezeLevel        = 0
  , symInfoTradeExemode            = SYMBOL_TRADE_EXECUTION_MARKET
  , symInfoSwapMode                = SYMBOL_SWAP_MODE_DISABLED
  , symInfoSwapRollover3days       = 3
  , symInfoMarginHedgedUseLeg      = True
  , symInfoExpirationMode          = 15
  , symInfoFillingMode             = 1
  , symInfoOrderMode               = 119
  , symInfoOrderGtcMode            = 0
  , symInfoOptionMode              = SYMBOL_OPTION_MODE_EUROPEAN
  , symInfoOptionRight             = SYMBOL_OPTION_RIGHT_CALL
  , symInfoBid                     = symbolInfoBid resp
  , symInfoBidhigh                 = 0.0
  , symInfoBidlow                  = 0.0
  , symInfoAsk                     = symbolInfoAsk resp
  , symInfoAskhigh                 = 0.0
  , symInfoAsklow                  = 0.0
  , symInfoLast                    = 0.0
  , symInfoLasthigh                = 0.0
  , symInfoLastlow                 = 0.0
  , symInfoVolumeReal              = 0.0
  , symInfoVolumehighReal          = 0.0
  , symInfoVolumelowReal           = 0.0
  , symInfoOptionStrike            = 0.0
  , symInfoPoint                   = symbolInfoPoint resp
  , symInfoTradeTickValue          = 0.0
  , symInfoTradeTickValueProfit    = 0.0
  , symInfoTradeTickValueLoss      = 0.0
  , symInfoTradeTickSize           = 0.01
  , symInfoTradeContractSize       = 100000.0
  , symInfoTradeAccruedInterest    = 0.0
  , symInfoTradeFaceValue          = 0.0
  , symInfoTradeLiquidityRate      = 0.0
  , symInfoVolumeMin               = 0.01
  , symInfoVolumeMax               = 25.0
  , symInfoVolumeStep              = 0.01
  , symInfoVolumeLimit             = 60.0
  , symInfoSwapLong                = 0.0
  , symInfoSwapShort               = 0.0
  , symInfoMarginInitial           = 0.0
  , symInfoMarginMaintenance       = 0.0
  , symInfoSessionVolume           = 0.0
  , symInfoSessionTurnover         = 0.0
  , symInfoSessionInterest         = 0.0
  , symInfoSessionBuyOrdersVolume  = 0.0
  , symInfoSessionSellOrdersVolume = 0.0
  , symInfoSessionOpen             = 0.0
  , symInfoSessionClose            = 0.0
  , symInfoSessionAw               = 0.0
  , symInfoSessionPriceSettlement  = 0.0
  , symInfoSessionPriceLimitMin    = 0.0
  , symInfoSessionPriceLimitMax    = 0.0
  , symInfoMarginHedged            = 0.0
  , symInfoPriceChange             = 0.0
  , symInfoPriceVolatility         = 0.0
  , symInfoPriceTheoretical        = 0.0
  , symInfoPriceGreeksDelta        = 0.0
  , symInfoPriceGreeksTheta        = 0.0
  , symInfoPriceGreeksGamma        = 0.0
  , symInfoPriceGreeksVega         = 0.0
  , symInfoPriceGreeksRho          = 0.0
  , symInfoPriceGreeksOmega        = 0.0
  , symInfoPriceSensitivity        = 0.0
  , symInfoBasis                   = ""
  , symInfoCategory                = ""
  , symInfoCurrencyBase            = ""
  , symInfoCurrencyProfit          = ""
  , symInfoCurrencyMargin          = ""
  , symInfoBank                    = ""
  , symInfoDescription             = T.unpack $ symbolInfoSymbol resp
  , symInfoExchange                = ""
  , symInfoFormula                 = ""
  , symInfoIsin                    = ""
  , symInfoName                    = T.unpack $ symbolInfoSymbol resp
  , symInfoPage                    = ""
  , symInfoPath                    = ""
  }

-- | Select a symbol in the MetaTrader 5 terminal.
--
symbolSelect :: Symbol          -- ^ Symbol name to select (e.g., "EURUSD")
             -> IO Bool
symbolSelect symbol = withMT5Lock $ do
  outcome <- try $ do
    send "SYMBOL_SELECT"
    send symbol
    unpickle' "Bool" <$> receive
  case outcome of
    Left (_ :: SomeException) -> return False
    Right res                 -> return res

-- | Get current price information for a trading symbol
--
-- This function retrieves real-time price data including bid, ask, spread,
-- volume and timestamp information for the specified symbol.
--
-- ==== __Examples__
--
-- >>> currentPriceGET "EURUSD"
-- Right CurrentPrice{cpSymbol="EURUSD", cpBid=1.0850, cpAsk=1.0852, cpSpread=0.0002, ...}
--
-- >>> currentPriceGET "INVALID_SYMBOL"
-- Left "No tick data available for INVALID_SYMBOL"
-- | Get current price information for a trading symbol.
--
-- Sends 'SYMBOL_INFO_TICK' command and reads bid, ask, last price, volume, and flags.
-- Returns 'Right CurrentPrice' on success, or 'Left error' if symbol not found.
--
-- ==== __Examples__
--
-- >>> currentPriceGET "EURUSD"
-- Right CurrentPrice{...}
--
-- >>> currentPriceGET "INVALID"
-- Left "No tick data available for INVALID"
currentPriceGET :: Symbol      -- ^ Trading symbol for price retrieval
                -> IO (Either String CurrentPrice)
currentPriceGET symbol = withMT5Lock $ do
  outcome <- try $ do
    -- Follow established command pattern (uppercase commands)
    send "SYMBOL_INFO_TICK"
    send symbol
    -- Read the response following the established pattern
    result <- unpickle' "String" <$> receive
    -- Check if response indicates an error
    if "error:" `isPrefixOf` result
      then return $ Left (drop 6 result) -- Remove "error:" prefix
      else parseCurrentPriceFromFields symbol
  case outcome of
    Left (e :: SomeException) -> return $ Left (show e)
    Right res                 -> return res

-- | Parse current price by reading individual fields from Python server
-- Following the established pattern of reading fields sequentially
parseCurrentPriceFromFields :: Symbol -> IO (Either String CurrentPrice)
parseCurrentPriceFromFields symbol = do
  bid        <- unpickle' "Double" <$> receive  -- bid price
  ask        <- unpickle' "Double" <$> receive  -- ask price
  lastPrice  <- unpickle' "Double" <$> receive  -- last price
  volume     <- unpickle' "Int" <$> receive     -- volume
  timeEpoch  <- unpickle' "Integer" <$> receive -- time (seconds)
  timeMsc    <- unpickle' "Integer" <$> receive -- time_msc (milliseconds)
  flags      <- unpickle' "Int" <$> receive     -- flags
  volReal    <- unpickle' "Double" <$> receive  -- volume_real

  let utcTime = secondsToUTCTime timeEpoch      -- Convert using existing utility
  let spread = ask - bid                        -- Calculate spread

  return $ Right $ CurrentPrice
    { cpSymbol     = T.pack symbol
    , cpBid        = bid
    , cpAsk        = ask
    , cpSpread     = spread
    , cpLast       = lastPrice
    , cpVolume     = volume
    , cpTime       = utcTime
    , cpTimeMsc    = timeMsc
    , cpFlags      = flags
    , cpVolumeReal = volReal
    }

-- | Check the validity of a trade request without sending it to the market.
--
-- Sends the 'ORDER_CHECK' command with the trade request and receives the result.
-- Returns an 'OrderSendResult' with the check outcome.
--
-- Corresponds to MetaTrader5.order_check().
-- | Check the validity of a trade request without executing it.
--
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for reliability).
--
-- Sends the 'ORDER_CHECK' command with the trade request and receives the check result.
-- Returns an 'OrderSendResult' detailing validity or errors.
--
-- Corresponds to MetaTrader5.order_check().
orderCheck :: MqlTradeRequest  -- ^ Trade request parameters to validate
           -> IO OrderSendResult
orderCheck request = do
  config <- getConfig
  case positionManagementChannel config of
    FileBridge           -> orderCheckViaFile request
    FileBridgeCustom _ _ -> orderCheckViaFile request
    PythonBridge         -> orderCheckViaPython request

-- | Check order using file bridge
orderCheckViaFile :: MqlTradeRequest -> IO OrderSendResult
orderCheckViaFile mqlReq = do
  let reqResult = mkOrderSendRequest
                    (trReqAction mqlReq)
                    (trReqMagic mqlReq)
                    (trReqOrder mqlReq)
                    (T.pack $ trReqSymbol mqlReq)
                    (trReqVolume mqlReq)
                    (trReqPrice mqlReq)
                    (trReqStoplimit mqlReq)
                    (trReqSl mqlReq)
                    (trReqTp mqlReq)
                    (trReqDeviation mqlReq)
                    (trReqType mqlReq)
                    (trReqTypeFilling mqlReq)
                    (trReqTypeTime mqlReq)
                    (trReqExpiration mqlReq)
                    (T.pack $ trReqComment mqlReq)
                    (trReqPosition mqlReq)
                    (trReqPositionBy mqlReq)
  case reqResult of
    Left err ->
      -- Validation failure: return error result with appropriate retcode
      let retcode = case err of
            Req.InvalidVolume _ -> TRADE_RETCODE_INVALID_VOLUME
            Req.InvalidPrice _  -> TRADE_RETCODE_INVALID_PRICE
            Req.InvalidSymbol _ -> TRADE_RETCODE_INVALID
            _                   -> TRADE_RETCODE_INVALID
          errMsg = "Invalid order check request: " ++ show err
      in return $ OrderSendResult retcode 0 0 0.0 0.0 0.0 0.0 errMsg 0 0
    Right req -> do
      mResponse <- sendRequestViaFile "order_check" req 5000
      case mResponse of
        Nothing ->
          -- Timeout: return error result with TRADE_RETCODE_TIMEOUT
          return $ OrderSendResult TRADE_RETCODE_TIMEOUT 0 0 0.0 0.0 0.0 0.0 "Order check request timed out" 0 0
        Just response -> do
          let mOrderCheck = decode (encode $ responseData response) :: Maybe OrderSendResponse
          case mOrderCheck of
            Nothing ->
              -- Parse failure: return error result with TRADE_RETCODE_ERROR
              return $ OrderSendResult TRADE_RETCODE_ERROR 0 0 0.0 0.0 0.0 0.0 "Failed to parse order check response" 0 0
            Just resp -> return $ convertOrderSendResponse resp

-- | Check order using Python bridge
orderCheckViaPython :: MqlTradeRequest -> IO OrderSendResult
orderCheckViaPython request = withMT5Lock $ do
  outcome <- try $ do
    send "ORDER_CHECK"
    sendMqlTradeRequest request
    readOrderSendResult
  case outcome of
    Left (e :: SomeException) ->
      return $ OrderSendResult TRADE_RETCODE_TIMEOUT 0 0 0.0 0.0 0.0 0.0
                 ("Order check cycle failed: " ++ show e) 0 0
    Right res -> return res

-- | Send a trade request to the Python server (internal helper).
--
-- Used by 'orderCheck' and 'orderSend' to transmit all fields of 'MqlTradeRequest'.
--
-- Not exposed to users.
sendMqlTradeRequest :: MqlTradeRequest -> IO ()
sendMqlTradeRequest MqlTradeRequest {..} = do
  send $ show . fromEnum $ trReqAction
  send $ show trReqMagic
  send $ show trReqOrder
  send   trReqSymbol
  send $ show trReqVolume
  send $ show . fromDecimalNumber $ trReqPrice
  send $ show . fromDecimalNumber $ trReqStoplimit
  send $ show . fromDecimalNumber $ trReqSl
  send $ show . fromDecimalNumber $ trReqTp
  send $ show trReqDeviation
  send $ show . fromEnum $ trReqType
  send $ show . fromEnum $ trReqTypeFilling
  send $ show . fromEnum $ trReqTypeTime
  send $ show trReqExpiration
  send   trReqComment
  send $ show trReqPosition
  send $ show trReqPositionBy

-- | Send a trade request to the market.
--
-- Sends the 'ORDER_SEND' command with the trade request and receives the result.
-- Returns an 'OrderSendResult' with the execution outcome.
--
-- Corresponds to MetaTrader5.order_send().
-- | Send a trade request to the market.
--
-- Uses 'ExceptT' for type-safe error handling with automatic error propagation.
--
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for broker requirements).
--
-- Sends the 'ORDER_SEND' command with the trade request and receives execution result.
--
-- **Errors**:
-- - 'TimeoutError': Request timed out after 5 seconds
-- - 'ParseError': Failed to parse response from MT5 EA
-- - 'BrokerError': MT5 returned error code (e.g., TRADE_RETCODE_INVALID)
-- - 'PythonProcessError': Python bridge communication failed
--
-- ==== __Examples__
--
-- >>> -- Send a buy order
-- >>> result <- runExceptT $ orderSend myBuyRequest
-- >>> case result of
-- >>>   Left err -> putStrLn $ "Order failed: " ++ show err
-- >>>   Right orderResult -> print orderResult
--
-- >>> -- Composition with automatic error propagation
-- >>> tradingOp = do
-- >>>   orders <- ordersGet Nothing Nothing
-- >>>   result <- orderSend myRequest
-- >>>   return (length orders, result)
-- >>> result <- runExceptT tradingOp
--
-- >>> -- Using compatibility wrapper
-- >>> result <- orderSendEither myRequest
--
-- Corresponds to MetaTrader5.order_send().
--
-- @since 0.2.0.0
--
orderSend :: MqlTradeRequest   -- ^ Trade request parameters to execute
          -> ExceptT MT5Error IO OrderSendResult
orderSend request = do
  config <- liftIO getConfig
  case positionManagementChannel config of
    FileBridge           -> orderSendViaFile request
    FileBridgeCustom _ _ -> orderSendViaFile request
    PythonBridge         -> orderSendViaPython request

-- | Send order using file bridge (broker restriction: MUST use EA)
--
-- This function routes requests to specialized handlers when appropriate:
-- - Position close: When position > 0, action = DEAL → routes to positionClose/positionClosePartial
-- - Position modify: When position > 0, action = SLTP → routes to positionModify
-- - Other cases: Uses generic OrderSend handler
orderSendViaFile :: MqlTradeRequest -> ExceptT MT5Error IO OrderSendResult
orderSendViaFile mqlReq = do
  -- Log incoming request for debugging (full details via show)
  liftIO $ $(logInfo) $ "[orderSendViaFile] Incoming MqlTradeRequest: " ++ show mqlReq

  -- Check if this should be routed to a specialized handler
  case (trReqAction mqlReq, trReqPosition mqlReq) of
    -- Position close: action=DEAL + position > 0
    (TRADE_ACTION_DEAL, pos) | pos > 0 -> do
      -- Need to determine if full or partial close by getting position info
      positions <- positionsGet
      
      -- Find the position to check its volume
      let mPosition = find (\p -> trPosTicket p == pos) positions
      case mPosition of
        Just position -> do
          let posVolume = fromDecimalNumber $ trPosVolume position
              reqVolume = fromDecimalNumber $ trReqVolume mqlReq
          -- Route to appropriate close function (both now in ExceptT context)
          success <- if abs (posVolume - reqVolume) < 0.0001  -- Close tolerance
                     then positionCloseViaFile pos
                     else positionClosePartialViaFile pos reqVolume
          -- Convert result to OrderSendResult
          return $ convertPositionCloseResult success
        Nothing ->
          -- Position not found - return error result via OrderSendResult
          return $ OrderSendResult TRADE_RETCODE_INVALID 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 ("Position not found: " ++ show pos) 0 0

    -- Position modify: action=SLTP + position > 0
    (TRADE_ACTION_SLTP, pos) | pos > 0 -> do
      let sl = fromDecimalNumber $ trReqSl mqlReq
          tp = fromDecimalNumber $ trReqTp mqlReq
      -- positionModifyViaFile now returns ExceptT, so use directly
      success <- positionModifyViaFile pos sl tp
      -- Convert result to OrderSendResult
      return $ convertPositionModifyResult success

    -- All other cases: use generic OrderSend handler
    _ -> do
      let reqResult = mkOrderSendRequest
                        (trReqAction mqlReq)
                        (trReqMagic mqlReq)
                        (trReqOrder mqlReq)
                        (T.pack $ trReqSymbol mqlReq)
                        (trReqVolume mqlReq)
                        (trReqPrice mqlReq)
                        (trReqStoplimit mqlReq)
                        (trReqSl mqlReq)
                        (trReqTp mqlReq)
                        (trReqDeviation mqlReq)
                        (trReqType mqlReq)
                        (trReqTypeFilling mqlReq)
                        (trReqTypeTime mqlReq)
                        (trReqExpiration mqlReq)
                        (T.pack $ trReqComment mqlReq)
                        (trReqPosition mqlReq)
                        (trReqPositionBy mqlReq)
      case reqResult of
        Left err ->
          -- Validation failure: return error result with appropriate retcode
          let retcode = case err of
                Req.InvalidVolume _ -> TRADE_RETCODE_INVALID_VOLUME
                Req.InvalidPrice _  -> TRADE_RETCODE_INVALID_PRICE
                Req.InvalidSymbol _ -> TRADE_RETCODE_INVALID
                _                   -> TRADE_RETCODE_INVALID
              errMsg = "Invalid order send request: " ++ show err
          in return $ OrderSendResult retcode 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 errMsg 0 0
        Right req -> do
          mResponse <- liftIO $ sendRequestViaFile "order_send" req 5000
          response <- liftMaybe (TimeoutError "order_send" 5000) mResponse
          
          let mOrderSend = decode (encode $ responseData response) :: Maybe OrderSendResponse
          orderSendResp <- liftResponseOrTypedError "OrderSendResponse" response mOrderSend
          return $ convertOrderSendResponse orderSendResp

-- | Convert position close result to OrderSendResult
convertPositionCloseResult :: Bool -> OrderSendResult
convertPositionCloseResult True =
  OrderSendResult TRADE_RETCODE_DONE 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Position closed successfully" 0 0
convertPositionCloseResult False =
  OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Position close failed" 0 0
-- convertPositionCloseResult (Left (TimeoutError action timeout)) =
--   OrderSendResult TRADE_RETCODE_TIMEOUT 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (T.unpack action ++ " timed out after " ++ show timeout ++ "ms") 0 0
-- convertPositionCloseResult (Left err) =
--   OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (show err) 0 0

-- | Convert position modify result to OrderSendResult
convertPositionModifyResult :: Bool -> OrderSendResult
convertPositionModifyResult True =
  OrderSendResult TRADE_RETCODE_DONE 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Position modified successfully" 0 0
convertPositionModifyResult False =
  OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Position modify failed" 0 0
-- convertPositionModifyResult (Left (TimeoutError action timeout)) =
--   OrderSendResult TRADE_RETCODE_TIMEOUT 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (T.unpack action ++ " timed out after " ++ show timeout ++ "ms") 0 0
-- convertPositionModifyResult (Left err) =
--   OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (show err) 0 0


-- | Send order using Python bridge (original implementation)
orderSendViaPython :: MqlTradeRequest -> ExceptT MT5Error IO OrderSendResult
orderSendViaPython request = liftIO $ withMT5Lock $ do
  send "ORDER_SEND"
  sendMqlTradeRequest request
  readOrderSendResult

-- | Convert OrderSendResponse (7 fields) to OrderSendResult (10 fields)
convertOrderSendResponse :: OrderSendResponse -> OrderSendResult
convertOrderSendResponse resp = OrderSendResult
  { ordSendRetcode          = intToTradeRetcode (orderSendRespRetcode resp)
  , ordSendDeal             = orderSendRespDeal resp
  , ordSendOrder            = orderSendRespOrder resp
  , ordSendVolume           = orderSendRespVolume resp
  , ordSendPrice            = orderSendRespPrice resp
  , ordSendBid              = 0.0  -- Not in EA response
  , ordSendAsk              = 0.0  -- Not in EA response
  , ordSendComment          = T.unpack $ orderSendRespComment resp
  , ordSendRequest_id       = 0    -- Not in EA response
  , ordSendRetcode_external = 0    -- Not in EA response
  }

-- | Convert Int to TradeRetcode (local helper)
intToTradeRetcode :: Int -> TradeRetcode
intToTradeRetcode x =
  case x of
    10004 -> TRADE_RETCODE_REQUOTE
    10006 -> TRADE_RETCODE_REJECT
    10007 -> TRADE_RETCODE_CANCEL
    10008 -> TRADE_RETCODE_PLACED
    10009 -> TRADE_RETCODE_DONE
    10010 -> TRADE_RETCODE_DONE_PARTIAL
    10011 -> TRADE_RETCODE_ERROR
    10012 -> TRADE_RETCODE_TIMEOUT
    10013 -> TRADE_RETCODE_INVALID
    10014 -> TRADE_RETCODE_INVALID_VOLUME
    10015 -> TRADE_RETCODE_INVALID_PRICE
    10016 -> TRADE_RETCODE_INVALID_STOPS
    10017 -> TRADE_RETCODE_TRADE_DISABLED
    10018 -> TRADE_RETCODE_MARKET_CLOSED
    10019 -> TRADE_RETCODE_NO_MONEY
    10020 -> TRADE_RETCODE_PRICE_CHANGED
    10021 -> TRADE_RETCODE_PRICE_OFF
    10022 -> TRADE_RETCODE_INVALID_EXPIRATION
    10023 -> TRADE_RETCODE_ORDER_CHANGED
    10024 -> TRADE_RETCODE_TOO_MANY_REQUESTS
    10025 -> TRADE_RETCODE_NO_CHANGES
    10026 -> TRADE_RETCODE_SERVER_DISABLES_AT
    10027 -> TRADE_RETCODE_CLIENT_DISABLES_AT
    10028 -> TRADE_RETCODE_LOCKED
    10029 -> TRADE_RETCODE_FROZEN
    10030 -> TRADE_RETCODE_INVALID_FILL
    10031 -> TRADE_RETCODE_CONNECTION
    10032 -> TRADE_RETCODE_ONLY_REAL
    10033 -> TRADE_RETCODE_LIMIT_ORDERS
    10034 -> TRADE_RETCODE_LIMIT_VOLUME
    10035 -> TRADE_RETCODE_INVALID_ORDER
    10036 -> TRADE_RETCODE_POSITION_CLOSED
    10038 -> TRADE_RETCODE_INVALID_CLOSE_VOLUME
    10039 -> TRADE_RETCODE_CLOSE_ORDER_EXIST
    10040 -> TRADE_RETCODE_LIMIT_POSITIONS
    10041 -> TRADE_RETCODE_REJECT_CANCEL
    10042 -> TRADE_RETCODE_LONG_ONLY
    10043 -> TRADE_RETCODE_SHORT_ONLY
    10044 -> TRADE_RETCODE_CLOSE_ONLY
    10045 -> TRADE_RETCODE_FIFO_CLOSE
    10046 -> TRADE_RETCODE_HEDGE_PROHIBITED
    _     -> TRADE_RETCODE_UNKNOWN

-- | Cancel a pending order by ticket number.
--
-- Routes through 'positionManagementChannel' from Config:
-- FileBridge: constructs TRADE_ACTION_REMOVE request via EA order_send action.
-- PythonBridge: sends ORDER_CANCEL command directly to Python server.
--
-- Corresponds to MetaTrader5.order_send() with TRADE_ACTION_REMOVE.
cancelOrderPOST :: Int            -- ^ Order ticket number to cancel
                -> IO OrderSendResult -- ^ Cancellation result
cancelOrderPOST orderTicket = do
  config <- getConfig
  case positionManagementChannel config of
    FileBridge           -> cancelOrderViaFile orderTicket
    FileBridgeCustom _ _ -> cancelOrderViaFile orderTicket
    PythonBridge         -> cancelOrderViaPython orderTicket

-- | Cancel pending order via file bridge using the dedicated order_cancel EA action.
-- Sends {"ticket": N} directly to HandleOrderCancel which calls trade.OrderDelete(ticket).
cancelOrderViaFile :: Int -> IO OrderSendResult
cancelOrderViaFile orderTicket = do
  let req = object ["ticket" .= orderTicket]
  mResponse <- sendRequestViaFile "order_cancel" req 5000
  case mResponse of
    Nothing ->
      return $ OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0
               ("Timeout waiting for order_cancel response for ticket: " ++ show orderTicket) 0 0
    Just resp -> do
      let mRetcode = parseMaybe (\v -> withObject "OrderCancelResponse" (\o -> o .: "retcode") v)
                                (responseData resp)
          retcode  = fromMaybe (-1 :: Int) mRetcode
      return $ if retcode == 10009
        then OrderSendResult TRADE_RETCODE_DONE  0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "" 0 0
        else OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0
               ("order_cancel retcode=" ++ show retcode) 0 0

-- | Cancel pending order via Python bridge (ORDER_CANCEL command).
cancelOrderViaPython :: Int -> IO OrderSendResult
cancelOrderViaPython orderTicket = withMT5Lock $ do
  outcome <- try $ do
    send "ORDER_CANCEL"
    send (show orderTicket)
    readOrderSendResult
  case outcome of
    Left (e :: SomeException) ->
      return $ OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0
                 ("order_cancel cycle failed for ticket " ++ show orderTicket ++ ": " ++ show e) 0 0
    Right res -> return res

-- | Cancel all pending orders in the account
--
-- Retrieves all pending orders and attempts to cancel each one individually.
-- Returns a list of cancellation results, one per order.
--
-- * Empty list if no pending orders exist
-- * Partial results if some cancellations fail
-- * Each result contains detailed information about the cancellation attempt
--
-- @since 0.1.0.0
cancelAllOrdersPOST :: IO [OrderSendResult]
cancelAllOrdersPOST = do
  config <- getConfig
  ordersResult <- runExceptT $ ordersGet Nothing Nothing
  case ordersResult of
    Left err ->
      return [OrderSendResult TRADE_RETCODE_INVALID_FILL 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 ("Failed to get orders: " ++ show err) 0 0]
    Right orders ->
      case positionManagementChannel config of
        FileBridge           -> mapM cancelTradeOrderViaFile orders
        FileBridgeCustom _ _ -> mapM cancelTradeOrderViaFile orders
        PythonBridge         -> mapM (cancelOrderViaPython . tradeOrderTicket) orders

-- | Cancel a pending order via FileBridge using its full TradeOrder record (avoids a second orders_get lookup).
cancelTradeOrderViaFile :: TradeOrder -> IO OrderSendResult
cancelTradeOrderViaFile order = do
  let sym = tradeOrderSymbol order
      orderTicket = tradeOrderTicket order
      cancelReq = MqlTradeRequest
        { trReqAction      = TRADE_ACTION_REMOVE
        , trReqMagic       = 0
        , trReqOrder       = fromIntegral orderTicket
        , trReqSymbol      = sym
        , trReqVolume      = DecimalNumber Nothing 0.0
        , trReqPrice       = DecimalNumber Nothing 0.0
        , trReqStoplimit   = DecimalNumber Nothing 0.0
        , trReqSl          = DecimalNumber Nothing 0.0
        , trReqTp          = DecimalNumber Nothing 0.0
        , trReqDeviation   = 0
        , trReqType        = ORDER_TYPE_BUY
        , trReqTypeFilling = ORDER_FILLING_RETURN
        , trReqTypeTime    = ORDER_TIME_GTC
        , trReqExpiration  = 0
        , trReqComment     = "Cancel order " ++ show orderTicket
        , trReqPosition    = 0
        , trReqPositionBy  = 0
        }
  result <- runExceptT $ orderSendViaFile cancelReq
  case result of
    Right r   -> return r
    Left  err -> return $ OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (show err) 0 0


-- | Get candlestick data using time range
--
-- Retrieves OHLC candlestick data for a specific time range using
-- MT5's copy_rates_range function. Following the same communication
-- pattern as currentPriceGET for consistency and reliability.
--
-- This function sends a COPY_RATES_RANGE command to the Python server
-- along with the symbol, timeframe, and date range parameters.
--
-- ==== __Examples__
--
-- >>> import Data.Time
-- >>> from <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2023-01-01 00:00:00"
-- >>> to <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2023-01-02 00:00:00"
-- >>> getCandleDataRange "EURUSD" M5 from to
-- Right (MT5CandleData {mt5Candles = [...], mt5Symbol = "EURUSD"})
--
-- >>> getCandleDataRange "INVALID" M5 from to
-- Left "No rate data available for INVALID, MT5 error: ..."
-- | Maximum number of bars fetched by a single underlying COPY_RATES_RANGE
-- call.  A large cold range (e.g. a multi-week historical backfill) fetched in
-- one call holds the daemon lock for its entire, possibly multi-minute,
-- duration — long enough to blow a live trader's per-cycle budget while the
-- process shares the daemon.  'getCandleDataRange' splits any wider request
-- into sub-ranges of at most this many bars, each its own bounded
-- 'withMT5Lock' cycle, so every lock hold stays short and a higher-priority
-- caller can preempt at the boundary between chunks.
maxBarsPerRangeCall :: Int
maxBarsPerRangeCall = 500

getCandleDataRange :: String          -- ^ Trading symbol (e.g., "EURUSD")
                   -> MT5Granularity  -- ^ Timeframe for candles
                   -> UTCTime         -- ^ Start time (inclusive)
                   -> UTCTime         -- ^ End time (inclusive)
                   -> IO (Either String MT5CandleData)
getCandleDataRange symbol granularity fromTime toTime
  | fromTime >= toTime = fetchRangeChunk symbol granularity fromTime toTime
  | otherwise          = go fromTime []
  where
    -- Duration one sub-range spans: at most 'maxBarsPerRangeCall' bars wide.
    chunkSpan :: NominalDiffTime
    chunkSpan = fromIntegral (maxBarsPerRangeCall * granularitySeconds granularity)

    -- One bar's worth of time; advances the cursor past a chunk's inclusive end
    -- so the next sub-range does not re-request the boundary bar.
    barStep :: NominalDiffTime
    barStep = fromIntegral (granularitySeconds granularity)

    -- Accumulate sub-range results (newest chunk first) until the far end is
    -- reached, then merge chronologically and drop any boundary duplicates.
    go :: UTCTime -> [[MT5Candle]] -> IO (Either String MT5CandleData)
    go cursor acc = do
      let chunkEnd = min toTime (addUTCTime chunkSpan cursor)
      res <- fetchRangeChunk symbol granularity cursor chunkEnd
      case res of
        Left err -> return (Left err)
        Right cd
          | chunkEnd >= toTime ->
              return $ Right $ MT5CandleData (mergeChunks (mt5Candles cd : acc)) symbol
          | otherwise ->
              go (addUTCTime barStep chunkEnd) (mt5Candles cd : acc)

    -- Chunks were pushed newest-first; reverse to chronological order, then
    -- drop candles sharing a timestamp with their predecessor (defensive
    -- against any inclusive-boundary overlap).
    mergeChunks :: [[MT5Candle]] -> [MT5Candle]
    mergeChunks = dedupAdjacent . concat . reverse

    dedupAdjacent :: [MT5Candle] -> [MT5Candle]
    dedupAdjacent (a : b : rest)
      | mt5CandleTime a == mt5CandleTime b = dedupAdjacent (a : rest)
      | otherwise                         = a : dedupAdjacent (b : rest)
    dedupAdjacent xs = xs

-- | Fetch a single COPY_RATES_RANGE window under one bounded 'withMT5Lock'
-- cycle.  The lock-hold duration of this call is bounded by 'maxBarsPerRangeCall'
-- via its sole caller 'getCandleDataRange'.
fetchRangeChunk :: String -> MT5Granularity -> UTCTime -> UTCTime -> IO (Either String MT5CandleData)
fetchRangeChunk symbol granularity fromTime toTime = withMT5Lock $ do
  outcome <- try $ do
    send "COPY_RATES_RANGE"
    send symbol
    send $ show $ toMT5TimeframeInt granularity
    send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fromTime
    send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" toTime
    -- Read response following established pattern (like currentPriceGET)
    result <- unpickle' "String" <$> receive
    if "error:" `isPrefixOf` result
      then return $ Left (drop 6 result)
      else parseCandleDataFromFields symbol
  case outcome of
    Left (e :: SomeException) -> return $ Left (show e)
    Right res                 -> return res

-- | Get candlestick data using count from specific time
--
-- Retrieves a specific number of candlesticks starting from a given time
-- using MT5's copy_rates_from function. Useful when you need a fixed
-- number of candles from a specific starting point.
--
-- ==== __Examples__
--
-- >>> import Data.Time
-- >>> from <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2023-01-01 00:00:00"
-- >>> getCandleDataFrom "EURUSD" M5 from 100
-- Right (MT5CandleData {mt5Candles = [...], mt5Symbol = "EURUSD"})
getCandleDataFrom :: String          -- ^ Trading symbol
                  -> MT5Granularity  -- ^ Timeframe for candles
                  -> UTCTime         -- ^ Start time
                  -> Int             -- ^ Number of candles to retrieve (max 5000)
                  -> IO (Either String MT5CandleData)
getCandleDataFrom symbol granularity fromTime count = withMT5Lock $ do
  outcome <- try $ do
    send "COPY_RATES_FROM"
    send symbol
    send $ show $ toMT5TimeframeInt granularity
    send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fromTime
    send $ show count
    result <- unpickle' "String" <$> receive
    if "error:" `isPrefixOf` result
      then return $ Left (drop 6 result)
      else parseCandleDataFromFields symbol
  case outcome of
    Left (e :: SomeException) -> return $ Left (show e)
    Right res                 -> return res

-- | Get recent candlestick data (most recent count candles)
--
-- Retrieves the most recent candlesticks using MT5's copy_rates_from_pos
-- function. This is the most efficient way to get recent market data
-- without specifying exact timestamps.
--
-- ==== __Examples__
--
-- >>> getCandleDataRecent "EURUSD" M5 50  -- Last 50 M5 candles
-- Right (MT5CandleData {mt5Candles = [...], mt5Symbol = "EURUSD"})
--
-- >>> getCandleDataRecent "GBPUSD" H1 24  -- Last 24 H1 candles (24 hours)
-- Right (MT5CandleData {mt5Candles = [...], mt5Symbol = "GBPUSD"})
getCandleDataRecent :: String          -- ^ Trading symbol
                    -> MT5Granularity  -- ^ Timeframe for candles
                    -> Int             -- ^ Number of recent candles (max 5000)
                    -> IO (Either String MT5CandleData)
getCandleDataRecent symbol granularity count = withMT5Lock $ do
  outcome <- try $ do
    send "COPY_RATES_FROM_POS"
    send symbol
    send $ show $ toMT5TimeframeInt granularity
    send "0"  -- start from most recent (position 0)
    send $ show count
    result <- unpickle' "String" <$> receive
    if "error:" `isPrefixOf` result
      then return $ Left (drop 6 result)
      else parseCandleDataFromFields symbol
  case outcome of
    Left (e :: SomeException) -> return $ Left (show e)
    Right res                 -> return res

-- | Parse candle data by reading individual fields from Python server
--
-- Following the established pattern of reading fields sequentially
-- (like parseCurrentPriceFromFields). This maintains consistency with
-- the existing communication protocol and ensures reliable data transfer.
--
-- The function reads the candle count first, then iterates to read
-- each candle's OHLC data sequentially, using the existing utility
-- functions for time conversion.
parseCandleDataFromFields :: String -> IO (Either String MT5CandleData)
parseCandleDataFromFields symbol = do
  candleCount <- unpickle' "Int" <$> receive
  candles <- replicateM candleCount readSingleCandle
  return $ Right $ MT5CandleData candles symbol
  where
    readSingleCandle :: IO MT5Candle
    readSingleCandle = MT5Candle
      <$> (secondsToUTCTime . unpickle' "Integer" <$> receive)  -- time (using existing utility)
      <*> (unpickle' "Double" <$> receive)                     -- open
      <*> (unpickle' "Double" <$> receive)                     -- high
      <*> (unpickle' "Double" <$> receive)                     -- low
      <*> (unpickle' "Double" <$> receive)                     -- close
      <*> (unpickle' "Int" <$> receive)                        -- tick_volume
      <*> (unpickle' "Int" <$> receive)                        -- spread
      <*> (unpickle' "Double" <$> receive)                     -- real_volume

-- | Retrieve ticks starting at a given UTC time, up to @count@ ticks.
--
-- Wraps MT5's @copy_ticks_from@. Use @copyTicksFlagsInfo@ (bid/ask quotes) or
-- @copyTicksFlagsAll@ (all tick types). Returns ticks in chronological order.
copyTicksFrom :: String    -- ^ Symbol (e.g. "US30.pro")
              -> UTCTime   -- ^ Start time (inclusive)
              -> Int       -- ^ Maximum number of ticks to return
              -> Int       -- ^ Tick type flags (see 'MT5.Data.Tick')
              -> IO (Either String [MT5Tick])
copyTicksFrom symbol fromTime count flags = withMT5Lock $ do
  outcome <- try $ do
    send "COPY_TICKS_FROM"
    send symbol
    send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fromTime
    send $ show count
    send $ show flags
    result <- unpickle' "String" <$> receive
    if "error:" `isPrefixOf` result
      then return $ Left (drop 6 result)
      else parseTickDataFromFields
  case outcome of
    Left (e :: SomeException) -> return $ Left (show e)
    Right res                 -> return res

-- | Retrieve all ticks within a UTC time range.
--
-- Wraps MT5's @copy_ticks_range@. Returns ticks in chronological order.
copyTicksRange :: String    -- ^ Symbol
               -> UTCTime   -- ^ Range start (inclusive)
               -> UTCTime   -- ^ Range end (inclusive)
               -> Int       -- ^ Tick type flags (see 'MT5.Data.Tick')
               -> IO (Either String [MT5Tick])
copyTicksRange symbol fromTime toTime flags = withMT5Lock $ do
  outcome <- try $ do
    send "COPY_TICKS_RANGE"
    send symbol
    send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fromTime
    send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" toTime
    send $ show flags
    result <- unpickle' "String" <$> receive
    if "error:" `isPrefixOf` result
      then return $ Left (drop 6 result)
      else parseTickDataFromFields
  case outcome of
    Left (e :: SomeException) -> return $ Left (show e)
    Right res                 -> return res

parseTickDataFromFields :: IO (Either String [MT5Tick])
parseTickDataFromFields = do
  tickCount <- unpickle' "Int" <$> receive
  ticks <- replicateM tickCount readSingleTick
  return $ Right ticks
  where
    readSingleTick :: IO MT5Tick
    readSingleTick = MT5Tick
      <$> (unpickle' "Integer" <$> receive)  -- time_msc
      <*> (unpickle' "Double"  <$> receive)  -- bid
      <*> (unpickle' "Double"  <$> receive)  -- ask
      <*> (unpickle' "Double"  <$> receive)  -- last
      <*> (unpickle' "Int"     <$> receive)  -- volume
      <*> (unpickle' "Int"     <$> receive)  -- flags
      <*> (unpickle' "Double"  <$> receive)  -- volume_real

-- | Net realized profit/loss for a closed position, summed across all of its
-- deals (opening and closing) as @profit + swap + commission + fee@.
--
-- Wraps MT5's @history_deals_get(position=ticket)@. The returned figure matches
-- the position's booked P&L in the MT5 terminal, unlike the last-observed
-- floating profit which excludes commission/swap and reflects the mid/opposite
-- quote at snapshot time rather than the actual fill.
--
-- Returns @Left@ with a message when the bridge reports an error, and
-- @Right 0.0@ when the position has no deals in loaded history.
historyDealsRealizedForPosition :: Integer  -- ^ MT5 position ticket
                                -> IO (Either String Double)
historyDealsRealizedForPosition positionId = withMT5Lock $ do
  outcome <- try $ do
    send "HISTORY_DEALS_GET_POSITION"
    send (show positionId)
    status <- unpickle' "String" <$> receive
    if "error:" `isPrefixOf` status
      then return $ Left (drop 6 status)
      else do
        n <- unpickle' "Int" <$> receive
        vals <- replicateM n $ do
          profit     <- unpickle' "Double" <$> receive
          swap       <- unpickle' "Double" <$> receive
          commission <- unpickle' "Double" <$> receive
          fee        <- unpickle' "Double" <$> receive
          (_entry :: Int) <- unpickle' "Int" <$> receive
          return (profit + swap + commission + fee)
        return $ Right (sum vals)
  case outcome of
    Left (e :: SomeException) -> return $ Left (show e)
    Right res                 -> return res
