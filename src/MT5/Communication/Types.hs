{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

-- | Shared types for MT5 communication (both Python bridge and file-based)
module MT5.Communication.Types
    ( Request(..)
    , Response(..)
    , ErrorResponse(..)
    ) where

import           Control.DeepSeq (NFData)
import           Data.Aeson      (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.:), (.=))
import           Data.Text       (Text)
import           GHC.Generics    (Generic)


-- | Request sent to MT5 (file-based or Python bridge)
data Request = Request
  { requestAction :: !Text   -- ^ Action to perform (e.g., "account_info", "positions_get")
  , requestData   :: !Value  -- ^ Action-specific data (JSON object)
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON Request where
  toJSON :: Request -> Value
  toJSON req = object
    [ "action" .= requestAction req
    , "data"   .= requestData req
    ]


-- | Response from MT5
data Response = Response
  { responseSuccess :: !Bool   -- ^ Whether the operation succeeded
  , responseData    :: !Value  -- ^ Response data (on success) or error details
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    success <- o .: "success"
    -- MT5 responses have different keys for success/error
    -- Success: {"success": true, "balance": 1000, ...}
    -- Error: {"success": false, "error_code": 404, "error_message": "..."}
    return $ Response success (Object o)


-- | Error response structure (when success = false)
data ErrorResponse = ErrorResponse
  { errorCode    :: !Int   -- ^ Error code
  , errorMessage :: !Text  -- ^ Human-readable error message
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \o ->
    ErrorResponse <$> o .: "error_code" <*> o .: "error_message"
