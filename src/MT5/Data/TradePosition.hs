module MT5.Data.TradePosition
    ( TradePosition(..)

    ) where

data TradePosition = TradePosition
  { trPosTicket          :: Int
  , trPosTime            :: Int
  , trPosTime_msc        :: Integer
  , trPosTime_update     :: Integer
  , trPosTime_update_msc :: Integer
  , trPosType            :: Int
  , trPosMagic           :: Int
  , trPosIdentifier      :: Int
  , trPosReason          :: Int
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
