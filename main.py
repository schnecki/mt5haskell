
import json
import time
import struct
import pickle
import sys
import os
from datetime import datetime, timezone
import matplotlib.pyplot as plt
import pandas as pd
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()
from mt5linux import MetaTrader5
# connecto to the server
mt5 = MetaTrader5(
    # host = 'localhost',
    # port = 18812
)
# import MetaTrader5 as mt5


# connect to MetaTrader 5

DEBUG=True

def send(x, info = ""):
    """Send object x and print info to console."""
    with os.fdopen(sys.stdout.fileno(), "ab", closefd = False) as stdout:
        # if info != "":
        if DEBUG:
            sys.stderr.write("Info: " + str(info) + ". Value: '" + str(x) + "' [Class: " + x.__class__.__name__ + "]\n")
            sys.stderr.flush()
        data = pickle.dumps(x, protocol = 2)
        length = struct.pack('!I', len(data))  # 4-byte big-endian unsigned int
        stdout.write(length)
        stdout.write(data)
        stdout.flush()


def sendLog(xs, prop: str):
    """Log and then send the property in xs."""
    # log(xs[prop])
    send(xs[prop], prop) # , info = prop)

account  = ""
password = ""
server   = "localhost"

def log(x):
    """Log x to console."""
    if DEBUG:
        sys.stderr.write(">> " + str(x) + "\n")
        sys.stderr.flush()
    return(x)

def getTradeRequest():
    """Retrieve a TradeRequest from the file descriptor."""
    action = int(sys.stdin.readline().strip())
    magic = int(sys.stdin.readline().strip())
    order = int(sys.stdin.readline().strip())
    symbol = sys.stdin.readline().strip()
    volume = float(sys.stdin.readline().strip())
    price = float(sys.stdin.readline().strip())
    stoplimit = float(sys.stdin.readline().strip())
    sl = float(sys.stdin.readline().strip())
    tp = float(sys.stdin.readline().strip())
    deviation = int(sys.stdin.readline().strip())
    type = int(sys.stdin.readline().strip())
    typeFilling = int(sys.stdin.readline().strip())
    typeTime = int(sys.stdin.readline().strip())
    expiration = int(sys.stdin.readline().strip())
    comment = sys.stdin.readline().strip()
    position = int(sys.stdin.readline().strip())
    positionBy = int(sys.stdin.readline().strip())
    request = {
       "action"       : action,
       "magic"        : magic,
       "order"        : order,
       "symbol"       : symbol,
       "volume"       : volume,
       "price"        : price,
       "stoplimit"    : stoplimit,
       "sl"           : sl,
       "tp"           : tp,
       "deviation"    : deviation,
       "type"         : type,
       "type_filling" : typeFilling,
       "type_time"    : typeTime,
       "expiration"   : expiration,
       "comment"      : comment,
       "position"     : position,
       "position_by"  : positionBy

    }
    return(request)


def sendOrders(orders):
    send(len(orders))
    for xs in orders:
        xs = xs._asdict()
        sendLog(xs, 'ticket')
        sendLog(xs, 'time_setup')
        sendLog(xs, 'time_setup_msc')
        sendLog(xs, 'time_expiration')
        sendLog(xs, 'type')
        sendLog(xs, 'type_time')
        sendLog(xs, 'type_filling')
        sendLog(xs, 'state')
        sendLog(xs, 'magic')
        sendLog(xs, 'volume_current')
        sendLog(xs, 'price_open')
        sendLog(xs, 'sl')
        sendLog(xs, 'tp')
        sendLog(xs, 'price_current')
        sendLog(xs, 'symbol')
        sendLog(xs, 'comment')
        sendLog(xs, 'external_id')


def sendSymbol(xs):
    xs = xs._asdict()
    sendLog(xs, 'custom')
    sendLog(xs, 'chart_mode')
    sendLog(xs, 'select')
    sendLog(xs, 'visible')
    sendLog(xs, 'session_deals')
    sendLog(xs, 'session_buy_orders')
    sendLog(xs, 'session_sell_orders')
    sendLog(xs, 'volume')
    sendLog(xs, 'volumehigh')
    sendLog(xs, 'volumelow')
    sendLog(xs, 'time')
    sendLog(xs, 'digits')
    sendLog(xs, 'spread')
    sendLog(xs, 'spread_float')
    sendLog(xs, 'ticks_bookdepth')
    sendLog(xs, 'trade_calc_mode')
    sendLog(xs, 'trade_mode')
    sendLog(xs, 'start_time')
    sendLog(xs, 'expiration_time')
    sendLog(xs, 'trade_stops_level')
    sendLog(xs, 'trade_freeze_level')
    sendLog(xs, 'trade_exemode')
    sendLog(xs, 'swap_mode')
    sendLog(xs, 'swap_rollover3days')
    sendLog(xs, 'margin_hedged_use_leg')
    sendLog(xs, 'expiration_mode')
    sendLog(xs, 'filling_mode')
    sendLog(xs, 'order_mode')
    sendLog(xs, 'order_gtc_mode')
    sendLog(xs, 'option_mode')
    sendLog(xs, 'option_right')
    sendLog(xs, 'bid')
    sendLog(xs, 'bidhigh')
    sendLog(xs, 'bidlow')
    sendLog(xs, 'ask')
    sendLog(xs, 'askhigh')
    sendLog(xs, 'asklow')
    sendLog(xs, 'last')
    sendLog(xs, 'lasthigh')
    sendLog(xs, 'lastlow')
    sendLog(xs, 'volume_real')
    sendLog(xs, 'volumehigh_real')
    sendLog(xs, 'volumelow_real')
    sendLog(xs, 'option_strike')
    sendLog(xs, 'point')
    sendLog(xs, 'trade_tick_value')
    sendLog(xs, 'trade_tick_value_profit')
    sendLog(xs, 'trade_tick_value_loss')
    sendLog(xs, 'trade_tick_size')
    sendLog(xs, 'trade_contract_size')
    sendLog(xs, 'trade_accrued_interest')
    sendLog(xs, 'trade_face_value')
    sendLog(xs, 'trade_liquidity_rate')
    sendLog(xs, 'volume_min')
    sendLog(xs, 'volume_max')
    sendLog(xs, 'volume_step')
    sendLog(xs, 'volume_limit')
    sendLog(xs, 'swap_long')
    sendLog(xs, 'swap_short')
    sendLog(xs, 'margin_initial')
    sendLog(xs, 'margin_maintenance')
    sendLog(xs, 'session_volume')
    sendLog(xs, 'session_turnover')
    sendLog(xs, 'session_interest')
    sendLog(xs, 'session_buy_orders_volume')
    sendLog(xs, 'session_sell_orders_volume')
    sendLog(xs, 'session_open')
    sendLog(xs, 'session_close')
    sendLog(xs, 'session_aw')
    sendLog(xs, 'session_price_settlement')
    sendLog(xs, 'session_price_limit_min')
    sendLog(xs, 'session_price_limit_max')
    sendLog(xs, 'margin_hedged')
    sendLog(xs, 'price_change')
    sendLog(xs, 'price_volatility')
    sendLog(xs, 'price_theoretical')
    sendLog(xs, 'price_greeks_delta')
    sendLog(xs, 'price_greeks_theta')
    sendLog(xs, 'price_greeks_gamma')
    sendLog(xs, 'price_greeks_vega')
    sendLog(xs, 'price_greeks_rho')
    sendLog(xs, 'price_greeks_omega')
    sendLog(xs, 'price_sensitivity')
    sendLog(xs, 'basis')
    sendLog(xs, 'category')
    sendLog(xs, 'currency_base')
    sendLog(xs, 'currency_profit')
    sendLog(xs, 'currency_margin')
    sendLog(xs, 'bank')
    sendLog(xs, 'description')
    sendLog(xs, 'exchange')
    sendLog(xs, 'formula')
    sendLog(xs, 'isin')
    sendLog(xs, 'name')
    sendLog(xs, 'page')
    sendLog(xs, 'path')


# request connection status and parameters
# get data on MetaTrader 5 version
# print(mt5.version())


for line in sys.stdin:
    if DEBUG:
        sys.stderr.write("Py Input: " + str(line) + "\n")
        sys.stderr.flush()
    line = line.rstrip().upper()
    if line == 'INITIALIZE':
        result = mt5.initialize()
        # log(mt5.terminal_info())
        # log(mt5.symbols_get())
        send(result)
    elif line == 'LOGIN':
        account = int(sys.stdin.readline().strip())
        password = sys.stdin.readline().strip()
        # server = sys.stdin.readline().strip()
        # timeout = sys.stdin.readline().strip()
        # sys.stderr.write("Account: " + str(account) + " pass: " + passw + "\n")
        send(log(mt5.login(login=account, password=password, server=server)))
        # send(mt5.login(account, password=passw))
    elif line == 'ACCOUNT_INFO':
        account_info=mt5.account_info()
        if account_info is None:
            send(log("account_info is None!"))
        else :
            xs = account_info._asdict()
            log(xs)
            sendLog(xs, 'login')              # 173941
            sendLog(xs, 'trade_mode')         # 2
            sendLog(xs, 'leverage')           # 100
            sendLog(xs, 'limit_orders')       # 0
            sendLog(xs, 'margin_so_mode')     # 0
            sendLog(xs, 'trade_allowed')      # False
            sendLog(xs, 'trade_expert')       # True
            sendLog(xs, 'margin_mode')        # 2
            sendLog(xs, 'currency_digits')    # 2
            sendLog(xs, 'fifo_close')         # False
            sendLog(xs, 'balance')            # 330.43
            sendLog(xs, 'credit')             # 0.0
            sendLog(xs, 'profit')             # 29.61
            sendLog(xs, 'equity')             # 360.04
            sendLog(xs, 'margin')             # 198.45
            sendLog(xs, 'margin_free')        # 161.59
            sendLog(xs, 'margin_level')       # 181.4260519022424
            sendLog(xs, 'margin_so_call')     # 100.0
            sendLog(xs, 'margin_so_so')       # 50.0
            sendLog(xs, 'margin_initial')     # 0.0
            sendLog(xs, 'margin_maintenance') # 0.0
            sendLog(xs, 'assets')             # 0.0
            sendLog(xs, 'liabilities')        # 0.0
            sendLog(xs, 'commission_blocked') # 0.0
            sendLog(xs, 'name')               # 'EU/M173941/EUR'
            sendLog(xs, 'server')             # 'OANDATMS-MT5'
            sendLog(xs, 'currency')           # 'EUR'
            sendLog(xs, 'company')            # 'OANDA TMS BrokersS.A.'
    elif line == 'POSITIONS_GET':
        log("Received command POSITIONS_GET")
        positions = mt5.positions_get()
        send(len(positions))
        for xs in positions:
            xs = xs._asdict()
            sendLog(xs, 'ticket')
            sendLog(xs, 'time')
            sendLog(xs, 'time_msc')
            sendLog(xs, 'time_update')
            sendLog(xs, 'time_update_msc')
            sendLog(xs, 'type')
            sendLog(xs, 'magic')
            sendLog(xs, 'identifier')
            sendLog(xs, 'reason')
            sendLog(xs, 'volume')
            sendLog(xs, 'price_open')
            sendLog(xs, 'sl')
            sendLog(xs, 'tp')
            sendLog(xs, 'price_current')
            sendLog(xs, 'swap')
            sendLog(xs, 'profit')
            sendLog(xs, 'symbol')
            sendLog(xs, 'comment')
            sendLog(xs, 'external_id')

    elif line == 'ORDERS_GET_SYMBOL':
        log("Received command ORDERS_GET_SYMBOL")
        symbol = sys.stdin.readline().strip()
        orders = mt5.orders_get(symbol = symbol)
        sendOrders(orders)
    elif line == 'ORDERS_GET_TICKET':
        log("Received command ORDERS_GET_TICKET")
        ticket = sys.stdin.readline().strip()
        orders = mt5.orders_get(ticket = ticket)
        sendOrders(orders)
    elif line == 'ORDERS_GET':
        log("Received command ORDERS_GET")
        orders = mt5.orders_get()
        sendOrders(orders)

    elif line == 'SYMBOLS_GET':
        group = sys.stdin.readline().strip()
        symbols = mt5.symbols_get(group)
        send(len(symbols), "Length")
        log(symbols)
        for xs in symbols:
            sendSymbol(xs)

    elif line == 'SYMBOL_SELECT':
        symbol = str(sys.stdin.readline().strip())
        log("Py Symbol: '" + str(symbol) + "'")
        result = mt5.symbol_select(symbol, True)
        send(log(result))
    elif line == 'SYMBOL_INFO':
        symbol = sys.stdin.readline().strip()
        result = mt5.symbol_info(symbol)
        sendSymbol(result)
    elif line == 'SYMBOL_INFO_TICK':
        symbol = sys.stdin.readline().strip()
        log(f"Received command SYMBOL_INFO_TICK for symbol: {symbol}")

        try:
            # Step 1: Ensure symbol is selected (following SYMBOL_SELECT pattern)
            if not mt5.symbol_select(symbol, True):
                error_msg = f"error:Failed to select symbol {symbol}"
                send(error_msg)
                continue

            # Step 2: Get tick information
            tick = mt5.symbol_info_tick(symbol)
            if tick is None:
                error_info = mt5.last_error()
                error_msg = f"error:No tick data available for {symbol}, MT5 error: {error_info}"
                send(error_msg)
                continue

            # Step 3: Send success indicator first (following established pattern)
            send("success")

            # Step 4: Send tick data fields individually (following sendLog pattern)
            send(tick.bid, "bid")
            send(tick.ask, "ask")
            send(tick.last, "last")
            send(tick.volume, "volume")
            send(tick.time, "time")
            send(tick.time_msc, "time_msc")
            send(tick.flags, "flags")
            send(tick.volume_real, "volume_real")

        except Exception as e:
            error_msg = f"error:Exception in SYMBOL_INFO_TICK: {str(e)}"
            send(error_msg)
    elif line == 'ORDER_CHECK' or line == 'ORDER_SEND':
        tr = getTradeRequest()
        if line == 'ORDER_SEND':
            result = mt5.order_send(tr)
        else:
            result = mt5.order_check(tr)
        xs = result._asdict()
        sendLog(xs, 'retcode')
        sendLog(xs, 'deal')
        sendLog(xs, 'order')
        sendLog(xs, 'volume')
        sendLog(xs, 'price')
        sendLog(xs, 'bid')
        sendLog(xs, 'ask')
        sendLog(xs, 'comment')
        sendLog(xs, 'request_id')
        sendLog(xs, 'retcode_external')

    elif line == 'ORDER_CANCEL':
        log("Received command ORDER_CANCEL")
        ticket = int(sys.stdin.readline().strip())
        cancel_request = {
            "action": mt5.TRADE_ACTION_REMOVE,
            "order": ticket,
        }
        result = mt5.order_send(cancel_request)
        xs = result._asdict()
        sendLog(xs, 'retcode')
        sendLog(xs, 'deal')
        sendLog(xs, 'order')
        sendLog(xs, 'volume')
        sendLog(xs, 'price')
        sendLog(xs, 'bid')
        sendLog(xs, 'ask')
        sendLog(xs, 'comment')
        sendLog(xs, 'request_id')
        sendLog(xs, 'retcode_external')

    elif line == 'COPY_RATES_RANGE':
        symbol = sys.stdin.readline().strip()
        timeframe = int(sys.stdin.readline().strip())
        date_from_str = sys.stdin.readline().strip()
        date_to_str = sys.stdin.readline().strip()

        log(f"Received command COPY_RATES_RANGE for symbol: {symbol}")

        try:
            # Parse dates
            date_from = datetime.strptime(date_from_str, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
            date_to = datetime.strptime(date_to_str, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)

            # Execute MT5 query
            rates = mt5.copy_rates_range(symbol, timeframe, date_from, date_to)
            if rates is None:
                error_info = mt5.last_error()
                error_msg = f"error:No rate data available for {symbol}, MT5 error: {error_info}"
                send(error_msg)
                continue

            # create DataFrame out of the obtained data
            rates_frame = pd.DataFrame(rates)
            rates_frame['time']=pd.to_datetime(rates_frame['time'], unit='s')
            log("rates: " + str(rates_frame))

            # Send success indicator (following SYMBOL_INFO_TICK pattern)
            send("success")

            # Send count of candles
            send(len(rates), "candle_count")

            # Send each candle individually (following sendLog pattern)
            # for idx in range(0, len(rates)):
            #     send(int(rates_frame.at[idx, 'time']), "time")         # timestamp
            #     send(float(rates_frame.at[idx, 'open']), "open")         # open price
            #     send(float(rates_frame.at[idx, 'high']), "high")         # high price
            #     send(float(rates_frame.at[idx, 'low']), "low")           # low price
            #     send(float(rates_frame.at[idx, 'close']), "close")       # close price
            #     send(int(rates_frame.at[idx, 'tick_volume']), "volume") # volume

            for rate in rates:
                send(int(rate['time']), "time")
                send(float(rate['open']), "open")
                send(float(rate['high']), "high")
                send(float(rate['low']), "low")
                send(float(rate['close']), "close")
                send(int(rate['tick_volume']), "volume")
                send(int(rate['spread']), "spread")
                send(float(rate['real_volume']), "real_volume")


        except Exception as e:
            error_msg = f"error:Exception in COPY_RATES_RANGE: {str(e)}"
            send(error_msg)

    elif line == 'COPY_RATES_FROM':
        symbol = sys.stdin.readline().strip()
        timeframe = int(sys.stdin.readline().strip())
        date_from_str = sys.stdin.readline().strip()
        count = int(sys.stdin.readline().strip())

        log(f"Received command COPY_RATES_FROM for symbol: {symbol}")

        try:
            date_from = datetime.strptime(date_from_str, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
            rates = mt5.copy_rates_from(symbol, timeframe, date_from, count)

            if rates is None:
                error_info = mt5.last_error()
                error_msg = f"error:No rate data available for {symbol}, MT5 error: {error_info}"
                send(error_msg)
                continue

            # create DataFrame out of the obtained data
            rates_frame = pd.DataFrame(rates)
            rates_frame['time']=pd.to_datetime(rates_frame['time'], unit='s')
            log("rates: " + str(rates_frame))

            send("success")
            send(len(rates), "candle_count")

            for rate in rates:
                send(int(rate['time']), "time")
                send(float(rate['open']), "open")
                send(float(rate['high']), "high")
                send(float(rate['low']), "low")
                send(float(rate['close']), "close")
                send(int(rate['tick_volume']), "volume")
                send(int(rate['spread']), "spread")
                send(float(rate['real_volume']), "real_volume")

        except Exception as e:
            error_msg = f"error:Exception in COPY_RATES_FROM: {str(e)}"
            send(error_msg)

    elif line == 'COPY_RATES_FROM_POS':
        symbol = sys.stdin.readline().strip()
        timeframe = int(sys.stdin.readline().strip())
        start_pos = int(sys.stdin.readline().strip())
        count = int(sys.stdin.readline().strip())

        log(f"Received command COPY_RATES_FROM_POS for symbol: {symbol}")

        try:
            rates = mt5.copy_rates_from_pos(symbol, timeframe, start_pos, count)

            if rates is None:
                error_info = mt5.last_error()
                error_msg = f"error:No rate data available for {symbol}, MT5 error: {error_info}"
                send(error_msg)
                continue

            # create DataFrame out of the obtained data
            rates_frame = pd.DataFrame(rates)
            rates_frame['time']=pd.to_datetime(rates_frame['time'], unit='s')
            log("rates: " + str(rates_frame))

            send("success")
            send(len(rates), "candle_count")

            for rate in rates:
                send(int(rate['time']), "time")
                send(float(rate['open']), "open")
                send(float(rate['high']), "high")
                send(float(rate['low']), "low")
                send(float(rate['close']), "close")
                send(int(rate['tick_volume']), "volume")
                send(int(rate['spread']), "spread")
                send(float(rate['real_volume']), "real_volume")

        except Exception as e:
            error_msg = f"error:Exception in COPY_RATES_FROM_POS: {str(e)}"
            send(error_msg)

    elif line == 'ERROR':
        formatString = sys.stdin.readline().strip()
        send(formatString.format(account, mt5.last_error()))
    elif line == 'QUIT':
        mt5.shutdown()
        send("Ciao!")
        break
    else:
        send("Unknown command: " + line)

    sys.stdout.flush()


mt5.shutdown()
