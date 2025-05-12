
module MT5.Data.OrderState
    ( OrderState (..)
    , toOrderState
    ) where


data OrderState =
       ORDER_STATE_STARTED
   |   ORDER_STATE_PLACED
   |   ORDER_STATE_CANCELED
   |   ORDER_STATE_PARTIAL
   |   ORDER_STATE_FILLED
   |   ORDER_STATE_REJECTED
   |   ORDER_STATE_EXPIRED
   |   ORDER_STATE_REQUEST_ADD
   |   ORDER_STATE_REQUEST_MODIFY
   |   ORDER_STATE_REQUEST_CANCEL
   deriving (Show, Eq, Ord, Enum)


toOrderState :: Int -> OrderState
toOrderState x = case x of
   0 -> ORDER_STATE_STARTED
   1 -> ORDER_STATE_PLACED
   2 -> ORDER_STATE_CANCELED
   3 -> ORDER_STATE_PARTIAL
   4 -> ORDER_STATE_FILLED
   5 -> ORDER_STATE_REJECTED
   6 -> ORDER_STATE_EXPIRED
   7 -> ORDER_STATE_REQUEST_ADD
   8 -> ORDER_STATE_REQUEST_MODIFY
   9 -> ORDER_STATE_REQUEST_CANCEL
   _ -> error $ "mt5haskell: Unknown order state " ++ show x
