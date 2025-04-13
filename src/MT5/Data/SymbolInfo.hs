module MT5.Data.SymbolInfo
    ( SymbolInfo (..)
    , readSymbolInfo
    ) where


import           MT5.Communication

data SymbolInfo = SymbolInfo
  { custom                     :: Bool   -- False
  , chart_mode                 :: Int    -- 1
  , select                     :: Bool   -- True
  , visible                    :: Bool   -- False
  , session_deals              :: Int    -- 0
  , session_buy_orders         :: Int    -- 0
  , session_sell_orders        :: Int    -- 0
  , volume                     :: Int    -- 0
  , volumehigh                 :: Int    -- 0
  , volumelow                  :: Int    -- 0
  , time                       :: Int    -- 1744412338
  , digits                     :: Int    -- 2
  , spread                     :: Int    -- 400
  , spread_float               :: Bool   -- False
  , ticks_bookdepth            :: Int    -- 0
  , trade_calc_mode            :: Int    -- 0
  , trade_mode                 :: Int    -- 0
  , start_time                 :: Int    -- 0
  , expiration_time            :: Int    -- 0
  , trade_stops_level          :: Int    -- 0
  , trade_freeze_level         :: Int    -- 0
  , trade_exemode              :: Int    -- 2
  , swap_mode                  :: Int    -- 0
  , swap_rollover3days         :: Int    -- 3
  , margin_hedged_use_leg      :: Bool   -- True
  , expiration_mode            :: Int    -- 15
  , filling_mode               :: Int    -- 1
  , order_mode                 :: Int    -- 119
  , order_gtc_mode             :: Int    -- 0
  , option_mode                :: Int    -- 0
  , option_right               :: Int    -- 0
  , bid                        :: Double -- 408.1
  , bidhigh                    :: Double -- 408.56
  , bidlow                     :: Double -- 404.68
  , ask                        :: Double -- 412.1
  , askhigh                    :: Double -- 412.56
  , asklow                     :: Double -- 408.68
  , last                       :: Double -- 410.1
  , lasthigh                   :: Double -- 410.56
  , lastlow                    :: Double -- 406.68
  , volume_real                :: Double -- 0.0
  , volumehigh_real            :: Double -- 0.0
  , volumelow_real             :: Double -- 0.0
  , option_strike              :: Double -- 0.0
  , point                      :: Double -- 0.01
  , trade_tick_value           :: Double -- 2.4265954865323947
  , trade_tick_value_profit    :: Double -- 2.4265954865323947
  , trade_tick_value_loss      :: Double -- 2.450379808870375
  , trade_tick_size            :: Double -- 0.01
  , trade_contract_size        :: Double -- 100000.0
  , trade_accrued_interest     :: Double -- 0.0
  , trade_face_value           :: Double -- 0.0
  , trade_liquidity_rate       :: Double -- 0.0
  , volume_min                 :: Double -- 0.01
  , volume_max                 :: Double -- 25.0
  , volume_step                :: Double -- 0.01
  , volume_limit               :: Double -- 60.0
  , swap_long                  :: Double -- 0.0
  , swap_short                 :: Double -- 0.0
  , margin_initial             :: Double -- 0.0
  , margin_maintenance         :: Double -- 0.0
  , session_volume             :: Double -- 0.0
  , session_turnover           :: Double -- 0.0
  , session_interest           :: Double -- 0.0
  , session_buy_orders_volume  :: Double -- 0.0
  , session_sell_orders_volume :: Double -- 0.0
  , session_open               :: Double -- 407.21
  , session_close              :: Double -- 407.02
  , session_aw                 :: Double -- 0.0
  , session_price_settlement   :: Double -- 0.0
  , session_price_limit_min    :: Double -- 0.0
  , session_price_limit_max    :: Double -- 0.0
  , margin_hedged              :: Double -- 0.0
  , price_change               :: Double -- 0.7567
  , price_volatility           :: Double -- 0.0
  , price_theoretical          :: Double -- 0.0
  , price_greeks_delta         :: Double -- 0.0
  , price_greeks_theta         :: Double -- 0.0
  , price_greeks_gamma         :: Double -- 0.0
  , price_greeks_vega          :: Double -- 0.0
  , price_greeks_rho           :: Double -- 0.0
  , price_greeks_omega         :: Double -- 0.0
  , price_sensitivity          :: Double -- 0.0
  , basis                      :: String -- ''
  , category                   :: String -- 'EURHUF Curncy'
  , currency_base              :: String -- 'EUR'
  , currency_profit            :: String -- 'HUF'
  , currency_margin            :: String -- 'EUR'
  , bank                       :: String -- ''
  , description                :: String -- 'EUR/HUF'
  , exchange                   :: String -- ''
  , formula                    :: String -- ''
  , isin                       :: String -- ''
  , name                       :: String -- 'EURHUF'
  , page                       :: String -- ''
  , path                       :: String -- 'Forex\\EURHUF'
  } deriving (Show, Eq)

readSymbolInfo :: IO SymbolInfo
readSymbolInfo =
  SymbolInfo
  <$> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Bool"   <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
  <*> (unpickle' "Int"    <$> receive)
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
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "String" <$> receive)
  <*> (unpickle' "String" <$> receive)
