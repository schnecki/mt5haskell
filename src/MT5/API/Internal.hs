{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Internal routing module for dual-channel MT5 communication.
--
-- This module provides the routing logic to support both:
-- 1. File-based communication (FileBridge) - uses MT5.Communication.File
-- 2. Python-based communication (PythonBridge) - uses MT5.Communication.Python
--
-- The channel is selected based on the global Config.communicationChannel setting.
module MT5.API.Internal
  ( sendRequestViaFile
  , channelSupportsRequest
  ) where

import           Control.Monad        (when)
import           Data.Aeson           (ToJSON, Value, toJSON)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import qualified Data.Text            as T

import           MT5.Communication.File  (getMT5FilesDirDefault, sendRequest, receiveResponse, initializeFiles)
import           MT5.Communication.Types (Request(..), Response(..))
import           MT5.Config              (Config(..), CommunicationChannel(..), getConfig)

-- | Send a request via file-based communication and receive the response.
--
-- This function:
-- 1. Gets the current Config to determine file paths
-- 2. Initializes request/response files (if needed)
-- 3. Sends the request as JSON
-- 4. Waits for response with timeout
-- 5. Returns the response data Value
--
-- Throws an error if:
-- - Config is not set to FileBridge
-- - Files cannot be created/accessed
-- - Request/response timeout occurs
-- - Response parsing fails
sendRequestViaFile :: ToJSON a
                   => Text              -- ^ Action name (e.g., "account_info")
                   -> a                 -- ^ Request data (will be converted to JSON)
                   -> Int               -- ^ Timeout in milliseconds
                   -> IO (Maybe Response)
sendRequestViaFile action requestData timeoutMs = do
  config <- getConfig
  
  case communicationChannel config of
    FileBridge -> do
      -- Get default MT5 files directory
      filesDir <- getMT5FilesDirDefault
      let reqPath = filesDir ++ "/mt5_api_request.json"
      let respPath = filesDir ++ "/mt5_api_response.json"
      
      -- Initialize files (creates them if they don't exist)
      initializeFiles reqPath respPath
      
      -- Create request
      let request = Request
            { requestAction = action
            , requestData = toJSON requestData
            }
      
      -- Send request and wait for response
      sendRequest reqPath request
      receiveResponse timeoutMs respPath
      
    FileBridgeCustom reqPath respPath -> do
      -- Initialize custom file paths
      initializeFiles reqPath respPath
      
      -- Create request
      let request = Request
            { requestAction = action
            , requestData = toJSON requestData
            }
      
      -- Send request and wait for response
      sendRequest reqPath request
      receiveResponse timeoutMs respPath
      
    PythonBridge ->
      error "sendRequestViaFile called with PythonBridge channel! Use sendRequestViaPython instead."

-- | Check if a communication channel supports a given request action.
--
-- This is useful for:
-- - Determining if a request can be handled by the current channel
-- - Providing helpful error messages when features are not supported
-- - Feature detection and fallback logic
--
-- FileBridge supports: account_info, positions_get, symbol_info, orders_get,
--   order_send, position_close, position_close_partial, position_modify,
--   symbol_select, symbols_get, candles_get
--
-- PythonBridge supports: All actions (legacy compatibility)
channelSupportsRequest :: CommunicationChannel -> Text -> Bool
channelSupportsRequest channel action =
  case channel of
    PythonBridge -> True  -- Python bridge supports everything (legacy)
    
    FileBridge -> action `elem` supportedFileBridgeActions
    
    FileBridgeCustom _ _ -> action `elem` supportedFileBridgeActions
  where
    -- Actions supported by the MT5RestAPIBridge.mq5 EA
    supportedFileBridgeActions =
      [ "account_info"
      , "positions_get"
      , "orders_get"
      , "order_send"
      , "order_check"
      , "order_cancel"
      , "symbol_info"
      , "symbol_select"
      , "symbols_get"
      , "candles_get"
      , "position_close"
      , "position_close_partial"
      , "position_modify"
      ]
