module MT5.Data.TradePosition
    ( TradePosition(..)
    , PositionReason(..)
    , PositionType(..)
    ) where

import           Data.Time.Clock (UTCTime)


data PositionReason
  = POSITION_REASON_CLIENT
  | POSITION_REASON_MOBILE
  | POSITION_REASON_WEB
  | POSITION_REASON_EXPERT
  deriving (Show, Eq, Ord, Enum)


data PositionType
  = POSITION_TYPE_BUY
  | POSITION_TYPE_SELL
  deriving (Show, Eq, Ord, Enum)


data TradePosition = TradePosition
  { trPosTicket          :: Int
  , trPosTime            :: UTCTime
  , trPosTime_msc        :: UTCTime
  , trPosTime_update     :: UTCTime
  , trPosTime_update_msc :: UTCTime
  , trPosType            :: PositionType
  , trPosMagic           :: Int
  , trPosIdentifier      :: Int
  , trPosReason          :: PositionReason
  , trPosVolume          :: Double
  , trPosPriceOpen       :: Double
  , trPosSl              :: Double
  , trPosTp              :: Double
  , trPosPrice_current   :: Double
  , trPosSwap            :: Double
  , trPosProfit          :: Double
  , trPosSymbol          :: String
  , trPosComment         :: String
  , trPosExternal_id     :: String
  } deriving (Show, Eq)
