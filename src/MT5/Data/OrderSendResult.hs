module MT5.Data.OrderSendResult
    ( OrderSendResult(..)
    , readOrderSendResult
    ) where

import           MT5.Communication

data OrderSendResult = OrderSendResult
  { ordSendRetcode          :: Int
  , ordSendDeal             :: Int
  , ordSendOrder            :: Int
  , ordSendVolume           :: Double
  , ordSendPrice            :: Double
  , ordSendBid              :: Double
  , ordSendAsk              :: Double
  , ordSendComment          :: String
  , ordSendRequest_id       :: Int
  , ordSendRetcode_external :: Int
  } deriving (Eq, Show)


readOrderSendResult :: IO OrderSendResult
readOrderSendResult =
  OrderSendResult
  <$> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "Double" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "Int" <$> receive)
  <*> (unpickle' "Int" <$> receive)
