module MT5.Data.TradeOrder
    ( TradeOrder(..)

    ) where

import           Data.Time.Clock     (UTCTime)

import           MT5.Data.OrderState
import           MT5.Data.OrderType

data TradeOrder = TradeOrder
  { tradeOrderTicket          :: Int
  , tradeOrderTime_setup      :: UTCTime -- ^ seconds
  , tradeOrderTime_setup_msc  :: UTCTime -- ^ miliseconds
  , tradeOrderTime_expiration :: Int
  , tradeOrderType            :: OrderType
  , tradeOrderType_time       :: Integer
  , tradeOrderType_filling    :: Int
  , tradeOrderState           :: OrderState
  , tradeOrderMagic           :: Int
  , tradeOrderVolume_current  :: Double
  , tradeOrderPrice_open      :: Double
  , tradeOrderSl              :: Double
  , tradeOrderTp              :: Double
  , tradeOrderPrice_current   :: Double
  , tradeOrderSymbol          :: String
  , tradeOrderComment         :: String
  , tradeOrderExternal_id     :: String
  } deriving (Show, Eq)


