{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module MT5.API
  ( SymbolGroup (..),
    initialize
  , loginAccount
  , accountInfo
  , positionsGet
  , positionClose
  , positionClosePartial
  , positionModify
  , symbolInfo
  , symbolsGet
  , symbolSelect
  , cancelOrderPOST
  , cancelAllOrdersPOST
  , orderCheck
  , orderSend
  , ordersGet
  , currentPriceGET
  , getCandleDataRange
  , getCandleDataFrom
  , getCandleDataRecent
  ) where

import           Control.DeepSeq
import           Control.Monad               (replicateM)
import           Data.Aeson                  (Value, decode, encode)
import qualified Data.ByteString.Lazy        as BSL
import           Data.List                   (filter, find, isPrefixOf)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime, getCurrentTime)
import           Data.Time.Format            (defaultTimeLocale, formatTime)
import           EasyLogger                  (logDebug, logInfo)
import           GHC.Generics                (Generic)
import           System.IO.Unsafe            (unsafePerformIO)


import           MT5.API.Internal            (sendRequestViaFile)
import           MT5.Communication           (receive, send, unpickle')
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
import           MT5.Communication.Types     (Response (..), responseData)
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
import           MT5.Data.DecimalNumber       (DecimalNumber(..), mkDecimalNumberFromDouble)
import           MT5.Data.AccountInfo        (AccountMarginMode (..),
                                              AccountStopoutMode (..),
                                              AccountTradeMode (..))
import           MT5.Data.Candle             (MT5Candle (MT5Candle),
                                              MT5CandleData (MT5CandleData))
import           MT5.Data.Granularity        (MT5Granularity, toMT5TimeframeInt)
import           MT5.Data.OrderSendResult    (TradeRetcode (..))
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
  send "INITIALIZE"
  res <- unpickle' "Bool" <$> receive
  if res
    then return $ Right ()
    else Left <$> getError "failed to initialize to account #{}, error code: {}"

-- | Log in to a MetaTrader 5 account.
--
-- Sends the 'LOGIN' command with account credentials to the Python server.
-- Returns 'Right ()' on success, or 'Left error' on failure.
--
loginAccount :: Login           -- ^ Account login credentials (username and password)
             -> IO (Either String ())
loginAccount Login {..} = do
  send "LOGIN"
  send account
  send password
  res <- unpickle' "Bool" <$> receive
  if res
    then return $ Right ()
    else Left <$> getError "failed to connect to account #{}, error code: {}"


-- | Retrieve account information for the current session.
--
-- **Default Routing**: Prefers PythonBridge (returns 28 complete fields) over FileBridge (14 fields = 50% data loss).
-- Config can override to FileBridge if explicitly needed, but PythonBridge strongly recommended for production.
--
-- Supports dual communication channels:
-- - FileBridge: Uses file-based communication with MT5RestAPIBridge.mq5 (faster but incomplete data)
-- - PythonBridge: Uses Python-based communication (complete data, recommended default)
--
-- Returns an 'AccountInfo' record with all account details.
--
-- Corresponds to MetaTrader5.account_info().
accountInfo :: IO AccountInfo
accountInfo = do
  config <- getConfig
  case communicationChannel config of
    FileBridge           -> accountInfoViaFile
    FileBridgeCustom _ _ -> accountInfoViaFile
    PythonBridge         -> accountInfoViaPython

-- | Get account info via file-based communication
accountInfoViaFile :: IO AccountInfo
accountInfoViaFile = do
  let req = mkAccountInfoRequest

  -- Send request and wait for response (5 second timeout)
  mResponse <- sendRequestViaFile "account_info" req 5000

  case mResponse of
    Nothing -> error "Account info request timed out"
    Just response -> do
      -- Parse the response data as AccountInfoResponse
      let mAccountInfo = decode (encode $ responseData response) :: Maybe AccountInfoResponse
      case mAccountInfo of
        Nothing      -> error "Failed to parse account info response"
        Just accResp -> return $ convertAccountInfoResponse accResp

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
accountInfoViaPython :: IO AccountInfo
accountInfoViaPython = do
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

-- | Request the last error message from the Python server.
--
-- Sends the 'ERROR' command with a format string and receives the error message.
--
-- Corresponds to MetaTrader5.last_error().
getError :: String             -- ^ Format string for the error request
         -> IO String
getError formatString = do
  send "ERROR"
  send formatString
  unpickle' "String" <$> receive


-- | Retrieve all open positions for the current account.
--
-- **Default Routing**: Prefers PythonBridge (returns 19 complete fields) over FileBridge (12 fields = 37% data loss).
-- Config can override to FileBridge if explicitly needed, but PythonBridge strongly recommended for production.
--
-- Supports dual communication channels:
-- - FileBridge: Uses file-based communication with MT5RestAPIBridge.mq5 (faster but incomplete data)
-- - PythonBridge: Uses Python-based communication (complete data, recommended default)
--
-- Returns a list of 'TradePosition' records.
--
-- Corresponds to MetaTrader5.positions_get().
positionsGet :: IO [TradePosition]
positionsGet = do
  config <- getConfig
  case positionManagementChannel config of
    FileBridge           -> positionsGetViaFile Nothing
    FileBridgeCustom _ _ -> positionsGetViaFile Nothing
    PythonBridge         -> positionsGetViaPython

-- | Get positions via file-based communication
positionsGetViaFile :: Maybe Symbol -> IO [TradePosition]
positionsGetViaFile mSymbol = do
  let req = mkPositionsGetRequest (fmap T.pack mSymbol)

  -- Send request and wait for response (5 second timeout)
  mResponse <- sendRequestViaFile "positions_get" req 5000

  case mResponse of
    Nothing -> error "Positions get request timed out"
    Just response -> do
      -- Parse the response data as PositionsGetResponse
      let mPositionsResp = decode (encode $ responseData response) :: Maybe PositionsGetResponse
      case mPositionsResp of
        Nothing -> error "Failed to parse positions get response"
        Just posResp -> return $ map convertPositionInfoResponse (positionsGetPositions posResp)

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
positionsGetViaPython :: IO [TradePosition]
positionsGetViaPython = do
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

-- | Close a position completely.
--
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for reliability).
--
-- For PythonBridge: Closes position by sending opposite order with position parameter set.
-- For FileBridge: Uses dedicated position_close EA action.
--
-- Corresponds to position_close EA action (FileBridge) or opposite order technique (PythonBridge).
--
-- Returns 'Right True' on successful close, 'Right False' if operation failed but was valid,
-- or 'Left error' for errors like timeout, invalid requests, or position not found.
positionClose :: Ticket -> IO (Either MT5Error Bool)
positionClose ticket = do
  config <- getConfig
  case positionManagementChannel config of
    FileBridge           -> positionCloseViaFile ticket
    FileBridgeCustom _ _ -> positionCloseViaFile ticket
    PythonBridge         -> positionCloseViaPython ticket

-- | Close a position via file-based communication
positionCloseViaFile :: Ticket -> IO (Either MT5Error Bool)
positionCloseViaFile ticket = do
  let eitherReq = mkPositionCloseRequest (fromIntegral ticket)
  case eitherReq of
    Left err -> return $ Left $ ValidationError $ T.pack $ "Invalid position close request: " ++ show err
    Right req -> do
      mResponse <- sendRequestViaFile "position_close" req 5000
      case mResponse of
        Nothing -> return $ Left $ TimeoutError "position_close" 5000
        Just response -> do
          let mResp = decode (encode $ responseData response) :: Maybe PositionCloseResponse
          case mResp of
            Nothing -> return $ Left $ ParseError "PositionCloseResponse" (T.pack $ show response)
            Just resp -> return $ Right $ positionCloseSuccess resp

-- | Close a position via Python bridge
--
-- Closes position by sending an opposite order with the position parameter set.
-- This is the standard MT5 method when mt5.Close() is not available.
positionCloseViaPython :: Ticket -> IO (Either MT5Error Bool)
positionCloseViaPython ticket = do
  -- First get the position to know its details
  positions <- positionsGet
  case filter (\p -> trPosTicket p == ticket) positions of
    [] -> return $ Left $ ValidationError $ T.pack $ "Position not found: " ++ show ticket
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
      return $ Right $ ordSendRetcode result == TRADE_RETCODE_DONE

-- | Close a position partially (reduce volume).
--
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for reliability).
--
-- For PythonBridge: Closes partial volume by sending opposite order with smaller volume.
-- For FileBridge: Uses dedicated position_close_partial EA action.
--
-- Corresponds to position_close_partial EA action.
--
-- Returns 'Right True' on successful partial close, 'Right False' if operation failed but was valid,
-- or 'Left error' for errors like timeout, invalid requests, or position not found.
positionClosePartial :: Ticket -> Double -> IO (Either MT5Error Bool)
positionClosePartial ticket volume = do
  config <- getConfig
  case positionManagementChannel config of
    FileBridge           -> positionClosePartialViaFile ticket volume
    FileBridgeCustom _ _ -> positionClosePartialViaFile ticket volume
    PythonBridge         -> positionClosePartialViaPython ticket volume

-- | Close position partially via file-based communication
positionClosePartialViaFile :: Ticket -> Double -> IO (Either MT5Error Bool)
positionClosePartialViaFile ticket volume = do
  -- Convert Double to DecimalNumber
  let volResult = mkDecimalNumberFromDouble volume
  case volResult of
    Left _ -> return $ Left $ ValidationError $ T.pack $ "Invalid volume: " ++ show volume
    Right vol -> do
      let eitherReq = mkPositionClosePartialRequest (fromIntegral ticket) vol
      case eitherReq of
        Left err -> return $ Left $ ValidationError $ T.pack $ "Invalid position close partial request: " ++ show err
        Right req -> do
          mResponse <- sendRequestViaFile "position_close_partial" req 5000
          case mResponse of
            Nothing -> return $ Left $ TimeoutError "position_close_partial" 5000
            Just response -> do
              let mResp = decode (encode $ responseData response) :: Maybe PositionCloseResponse
              case mResp of
                Nothing -> return $ Left $ ParseError "PositionCloseResponse" (T.pack $ show response)
                Just resp -> return $ Right $ positionCloseSuccess resp

-- | Modify a position's stop-loss and take-profit levels.
--
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for reliability).
--
-- For PythonBridge: Modifies position SL/TP using TRADE_ACTION_SLTP.
-- For FileBridge: Uses dedicated position_modify EA action.
--
-- Corresponds to position_modify EA action.
--
-- Returns 'Right True' on successful modification, 'Right False' if operation failed but was valid,
-- or 'Left error' for errors like timeout, invalid requests, or position not found.
positionModify :: Ticket -> Double -> Double -> IO (Either MT5Error Bool)
positionModify ticket sl tp = do
  config <- getConfig
  case positionManagementChannel config of
    FileBridge           -> positionModifyViaFile ticket sl tp
    FileBridgeCustom _ _ -> positionModifyViaFile ticket sl tp
    PythonBridge         -> positionModifyViaPython ticket sl tp

-- | Modify position via file-based communication
positionModifyViaFile :: Ticket -> Double -> Double -> IO (Either MT5Error Bool)
positionModifyViaFile ticket sl tp = do
  let eitherReq = mkPositionModifyRequest (fromIntegral ticket) sl tp
  case eitherReq of
    Left err -> return $ Left $ ValidationError $ T.pack $ "Invalid position modify request: " ++ show err
    Right req -> do
      mResponse <- sendRequestViaFile "position_modify" req 5000
      case mResponse of
        Nothing -> return $ Left $ TimeoutError "position_modify" 5000
        Just response -> do
          let mResp = decode (encode $ responseData response) :: Maybe PositionModifyResponse
          case mResp of
            Nothing -> return $ Left $ ParseError "PositionModifyResponse" (T.pack $ show response)
            Just resp -> return $ Right $ positionModifySuccess resp

-- | Close partial position via Python bridge
positionClosePartialViaPython :: Ticket -> Double -> IO (Either MT5Error Bool)
positionClosePartialViaPython ticket volume = do
  -- Convert Double to DecimalNumber
  let volResult = mkDecimalNumberFromDouble volume
  case volResult of
    Left _ -> return $ Left $ ValidationError $ T.pack $ "Invalid volume: " ++ show volume
    Right vol -> do
      -- First get the position to know its details
      positions <- positionsGet
      case filter (\p -> trPosTicket p == ticket) positions of
        [] -> return $ Left $ ValidationError $ T.pack $ "Position not found: " ++ show ticket
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
          return $ Right $ ordSendRetcode result == TRADE_RETCODE_DONE

-- | Modify position via Python bridge
positionModifyViaPython :: Ticket -> Double -> Double -> IO (Either MT5Error Bool)
positionModifyViaPython ticket sl tp = do
  -- Get position to know its symbol
  positions <- positionsGet
  case filter (\p -> trPosTicket p == ticket) positions of
    [] -> return $ Left $ ValidationError $ T.pack $ "Position not found: " ++ show ticket
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
      return $ Right $ ordSendRetcode result == TRADE_RETCODE_DONE

-- | Get active orders with the ability to filter by symbol or ticket.
--
-- **Default Routing**: Prefers PythonBridge (returns 17 complete fields) over FileBridge (10 fields = 41% data loss).
-- Config can override to FileBridge if explicitly needed, but PythonBridge strongly recommended for production.
--
-- **Note**: Ticket filtering only works with PythonBridge (FileBridge ignores ticket parameter).
--
-- Supports dual communication channels:
-- - FileBridge: Uses file-based communication with MT5RestAPIBridge.mq5 (faster but incomplete data, symbol filter only)
-- - PythonBridge: Uses Python-based communication (complete data + ticket filter, recommended default)
--
ordersGet :: Maybe Symbol      -- ^ Optional symbol filter (e.g., Just "EURUSD")
          -> Maybe Ticket      -- ^ Optional ticket filter (e.g., Just 12345) - PythonBridge only
          -> IO [TradeOrder]
ordersGet mInstr mTicket = do
  config <- getConfig
  case communicationChannel config of
    FileBridge           -> ordersGetViaFile mInstr mTicket
    FileBridgeCustom _ _ -> ordersGetViaFile mInstr mTicket
    PythonBridge         -> ordersGetViaPython mInstr mTicket

-- | Retrieve orders using file bridge (Note: ticket filter not supported by EA)
ordersGetViaFile :: Maybe Symbol -> Maybe Ticket -> IO [TradeOrder]
ordersGetViaFile mSymbol _mTicket = do
  let req = mkOrdersGetRequest (fmap T.pack mSymbol)
  mResponse <- sendRequestViaFile "orders_get" req 5000
  case mResponse of
    Nothing -> error "Orders get request timed out"
    Just response -> do
      let mOrders = decode (encode $ responseData response) :: Maybe OrdersGetResponse
      case mOrders of
        Nothing -> error "Failed to parse orders response"
        Just resp -> return $ map convertOrderInfoResponse (ordersGetOrders resp)

-- | Retrieve orders using Python bridge (original implementation)
ordersGetViaPython :: Maybe Symbol -> Maybe Ticket -> IO [TradeOrder]
ordersGetViaPython mInstr mTicket = do
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
symbolsGet mGroup =
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
-- **CRITICAL - Default Routing**: ALWAYS use PythonBridge in production!
-- FileBridge returns only 7 of 104 fields (93% data loss) which may cause order rejections.
-- Missing: contract size, tick value, volume limits, swap rates, margin requirements, ALL options data.
--
-- **Default Routing**: Prefers PythonBridge (returns 104 complete fields) over FileBridge (7 fields = 93% data loss).
-- Config can override to FileBridge but this is STRONGLY DISCOURAGED for production use.
--
-- Supports dual communication channels:
-- - FileBridge: Uses file-based communication with MT5RestAPIBridge.mq5 (INCOMPLETE - only 7 basic fields)
-- - PythonBridge: Uses Python-based communication (COMPLETE - all 104 fields, REQUIRED for production)
--
symbolInfo :: Symbol            -- ^ Symbol name to query (e.g., "EURUSD")
           -> IO SymbolInfo
symbolInfo symbol = do
  config <- getConfig
  case communicationChannel config of
    FileBridge           -> symbolInfoViaFile symbol
    FileBridgeCustom _ _ -> symbolInfoViaFile symbol
    PythonBridge         -> symbolInfoViaPython symbol

-- | Retrieve symbol information using file bridge
symbolInfoViaFile :: Symbol -> IO SymbolInfo
symbolInfoViaFile symbol = do
  let reqResult = mkSymbolInfoRequest (T.pack symbol)
  case reqResult of
    Left err -> error $ "Invalid symbol request: " ++ show err
    Right req -> do
      mResponse <- sendRequestViaFile "symbol_info" req 5000
      case mResponse of
        Nothing -> error $ "Symbol info request timed out for symbol: " ++ symbol
        Just response -> do
          let mSymbolInfo = decode (encode $ responseData response) :: Maybe SymbolInfoResponse
          case mSymbolInfo of
            Nothing -> error $ "Failed to parse symbol info response for: " ++ symbol
            Just resp -> return $ convertSymbolInfoResponse resp

-- | Retrieve symbol information using Python bridge (original implementation)
symbolInfoViaPython :: Symbol -> IO SymbolInfo
symbolInfoViaPython symbol = do
  send "SYMBOL_INFO"
  send symbol
  readSymbolInfo

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
symbolSelect symbol = do
  send "SYMBOL_SELECT"
  send symbol
  unpickle' "Bool" <$> receive

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
currentPriceGET symbol = do
  -- Follow established command pattern (uppercase commands)
  send "SYMBOL_INFO_TICK"
  send symbol

  -- Read the response following the established pattern
  result <- unpickle' "String" <$> receive

  -- Check if response indicates an error
  if "error:" `isPrefixOf` result
    then return $ Left (drop 6 result) -- Remove "error:" prefix
    else parseCurrentPriceFromFields symbol

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
orderCheckViaPython request = do
  send "ORDER_CHECK"
  sendMqlTradeRequest request
  readOrderSendResult

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
-- **Uses positionManagementChannel** from Config (defaults to FileBridge for broker requirements).
--
-- Sends the 'ORDER_SEND' command with the trade request and receives execution result.
-- Returns an 'OrderSendResult' with trade execution details.
--
-- Corresponds to MetaTrader5.order_send().
orderSend :: MqlTradeRequest   -- ^ Trade request parameters to execute
          -> IO OrderSendResult
orderSend request = do
  config <- getConfig
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
orderSendViaFile :: MqlTradeRequest -> IO OrderSendResult
orderSendViaFile mqlReq = do
  -- Log incoming request for debugging (full details via show)
  $(logInfo) $ "[orderSendViaFile] Incoming MqlTradeRequest: " ++ show mqlReq
  
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
          -- Route to appropriate close function
          closeResult <- if abs (posVolume - reqVolume) < 0.0001  -- Close tolerance
            then positionCloseViaFile pos
            else positionClosePartialViaFile pos reqVolume
          -- Convert result to OrderSendResult
          return $ convertPositionCloseResult closeResult
        Nothing ->
          -- Position not found
          return $ OrderSendResult TRADE_RETCODE_INVALID 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 ("Position not found: " ++ show pos) 0 0

    -- Position modify: action=SLTP + position > 0
    (TRADE_ACTION_SLTP, pos) | pos > 0 -> do
      let sl = fromDecimalNumber $ trReqSl mqlReq
          tp = fromDecimalNumber $ trReqTp mqlReq
      modifyResult <- positionModifyViaFile pos sl tp
      -- Convert result to OrderSendResult
      return $ convertPositionModifyResult modifyResult

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
          mResponse <- sendRequestViaFile "order_send" req 5000
          case mResponse of
            Nothing ->
              -- Timeout: return error result with TRADE_RETCODE_TIMEOUT
              return $ OrderSendResult TRADE_RETCODE_TIMEOUT 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Order send request timed out" 0 0
            Just response -> do
              let mOrderSend = decode (encode $ responseData response) :: Maybe OrderSendResponse
              case mOrderSend of
                Nothing ->
                  -- Parse failure: return error result with TRADE_RETCODE_ERROR
                  return $ OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Failed to parse order send response" 0 0
                Just resp -> return $ convertOrderSendResponse resp

-- | Convert position close result to OrderSendResult
convertPositionCloseResult :: Either MT5Error Bool -> OrderSendResult
convertPositionCloseResult (Right True) =
  OrderSendResult TRADE_RETCODE_DONE 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Position closed successfully" 0 0
convertPositionCloseResult (Right False) =
  OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Position close failed" 0 0
convertPositionCloseResult (Left (TimeoutError action timeout)) =
  OrderSendResult TRADE_RETCODE_TIMEOUT 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (T.unpack action ++ " timed out after " ++ show timeout ++ "ms") 0 0
convertPositionCloseResult (Left err) =
  OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (show err) 0 0

-- | Convert position modify result to OrderSendResult
convertPositionModifyResult :: Either MT5Error Bool -> OrderSendResult
convertPositionModifyResult (Right True) =
  OrderSendResult TRADE_RETCODE_DONE 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Position modified successfully" 0 0
convertPositionModifyResult (Right False) =
  OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 "Position modify failed" 0 0
convertPositionModifyResult (Left (TimeoutError action timeout)) =
  OrderSendResult TRADE_RETCODE_TIMEOUT 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (T.unpack action ++ " timed out after " ++ show timeout ++ "ms") 0 0
convertPositionModifyResult (Left err) =
  OrderSendResult TRADE_RETCODE_ERROR 0 0 (DecimalNumber Nothing 0.0) 0.0 0.0 0.0 (show err) 0 0


-- | Send order using Python bridge (original implementation)
orderSendViaPython :: MqlTradeRequest -> IO OrderSendResult
orderSendViaPython request = do
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

-- | Cancel a pending order by ticket number
--
-- Sends a cancellation request for the specified order ticket.
-- Only works for pending orders that haven't been executed yet.
--
-- Returns an 'OrderSendResult' with the cancellation outcome.
--
-- Corresponds to MetaTrader5.order_send() with TRADE_ACTION_REMOVE.
-- | Cancel a pending order by ticket number.
--
-- Sends the 'ORDER_CANCEL' command with the specified ticket number.
-- Returns an 'OrderSendResult' indicating cancellation outcome.
--
-- Corresponds to MetaTrader5.order_send() with TRADE_ACTION_REMOVE.
cancelOrderPOST :: Int            -- ^ Order ticket number to cancel
                -> IO OrderSendResult -- ^ Cancellation result
cancelOrderPOST orderTicket = do
  send "ORDER_CANCEL"
  send (show orderTicket)
  readOrderSendResult

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
  -- Phase 1: Get all pending orders
  orders <- ordersGet Nothing Nothing

  -- Phase 2: Cancel each order individually
  mapM cancelSingleOrder orders
  where
    cancelSingleOrder :: TradeOrder -> IO OrderSendResult
    cancelSingleOrder order = cancelOrderPOST (tradeOrderTicket order)


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
getCandleDataRange :: String          -- ^ Trading symbol (e.g., "EURUSD")
                   -> MT5Granularity  -- ^ Timeframe for candles
                   -> UTCTime         -- ^ Start time (inclusive)
                   -> UTCTime         -- ^ End time (inclusive)
                   -> IO (Either String MT5CandleData)
getCandleDataRange symbol granularity fromTime toTime = do
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
getCandleDataFrom symbol granularity fromTime count = do
  send "COPY_RATES_FROM"
  send symbol
  send $ show $ toMT5TimeframeInt granularity
  send $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fromTime
  send $ show count

  result <- unpickle' "String" <$> receive

  if "error:" `isPrefixOf` result
    then return $ Left (drop 6 result)
    else parseCandleDataFromFields symbol

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
getCandleDataRecent symbol granularity count = do
  send "COPY_RATES_FROM_POS"
  send symbol
  send $ show $ toMT5TimeframeInt granularity
  send "0"  -- start from most recent (position 0)
  send $ show count

  result <- unpickle' "String" <$> receive

  if "error:" `isPrefixOf` result
    then return $ Left (drop 6 result)
    else parseCandleDataFromFields symbol

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
