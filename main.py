
import json
import time
import struct
import pickle
import sys
import os
import socket
import threading
from datetime import datetime, timezone
import matplotlib.pyplot as plt
import pandas as pd
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()
from mt5linux import MetaTrader5
mt5 = MetaTrader5()

DEBUG=False
SOCKET_PATH = '/tmp/mt5haskell.sock'

# Global MT5 lock: MT5 API is not thread-safe; serialise all calls.
_mt5_lock = threading.Lock()
# Thread-local storage for per-connection I/O handles.
_tlocal = threading.local()


def _rl():
    """Read one stripped line from the current client connection."""
    line = _tlocal.fin.readline()
    if not line:
        raise EOFError('client disconnected')
    return line.strip()


def send(x, info = ""):
    """Send pickle-encoded value with 4-byte length prefix to current client."""
    if DEBUG:
        sys.stderr.write("Info: " + str(info) + ". Value: '" + str(x) + "' [Class: " + x.__class__.__name__ + "]\n")
        sys.stderr.flush()
    data = pickle.dumps(x, protocol = 2)
    length = struct.pack('!I', len(data))  # 4-byte big-endian unsigned int
    _tlocal.conn.sendall(length + data)


def sendLog(xs, prop: str):
    """Log and then send the property in xs."""
    send(xs[prop], prop)

account  = ""
password = ""
server   = "localhost"

def log(x):
    """Log x to stderr."""
    if DEBUG:
        sys.stderr.write(">> " + str(x) + "\n")
        sys.stderr.flush()
    return(x)

def getTradeRequest():
    """Retrieve a TradeRequest from the current client connection."""
    action = int(_rl())
    magic = int(_rl())
    order = int(_rl())
    symbol = _rl()
    volume = float(_rl())
    price = float(_rl())
    stoplimit = float(_rl())
    sl = float(_rl())
    tp = float(_rl())
    deviation = int(_rl())
    type = int(_rl())
    typeFilling = int(_rl())
    typeTime = int(_rl())
    expiration = int(_rl())
    comment = _rl()
    position = int(_rl())
    positionBy = int(_rl())
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
    if orders is None:
        orders = ()
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


def _handle_client(conn):
    """Handle a single client connection in its own thread."""
    _tlocal.conn = conn
    _tlocal.fin  = conn.makefile('r', encoding='utf-8')
    try:
        while True:
            line = _tlocal.fin.readline()
            if not line:
                break  # client closed connection
            if DEBUG:
                sys.stderr.write('Py Input: ' + str(line) + '\n')
                sys.stderr.flush()
            line = line.rstrip().upper()
            with _mt5_lock:
                if line == 'INITIALIZE':
                    result = mt5.initialize()
                    send(result)
                elif line == 'LOGIN':
                    account = int(_rl())
                    password = _rl()
                    send(log(mt5.login(login=account, password=password, server=server)))
                elif line == 'ACCOUNT_INFO':
                    account_info=mt5.account_info()
                    if account_info is None:
                        send(log('account_info is None!'))
                    else :
                        xs = account_info._asdict()
                        log(xs)
                        sendLog(xs, 'login')
                        sendLog(xs, 'trade_mode')
                        sendLog(xs, 'leverage')
                        sendLog(xs, 'limit_orders')
                        sendLog(xs, 'margin_so_mode')
                        sendLog(xs, 'trade_allowed')
                        sendLog(xs, 'trade_expert')
                        sendLog(xs, 'margin_mode')
                        sendLog(xs, 'currency_digits')
                        sendLog(xs, 'fifo_close')
                        sendLog(xs, 'balance')
                        sendLog(xs, 'credit')
                        sendLog(xs, 'profit')
                        sendLog(xs, 'equity')
                        sendLog(xs, 'margin')
                        sendLog(xs, 'margin_free')
                        sendLog(xs, 'margin_level')
                        sendLog(xs, 'margin_so_call')
                        sendLog(xs, 'margin_so_so')
                        sendLog(xs, 'margin_initial')
                        sendLog(xs, 'margin_maintenance')
                        sendLog(xs, 'assets')
                        sendLog(xs, 'liabilities')
                        sendLog(xs, 'commission_blocked')
                        sendLog(xs, 'name')
                        sendLog(xs, 'server')
                        sendLog(xs, 'currency')
                        sendLog(xs, 'company')
                elif line == 'POSITIONS_GET':
                    log('Received command POSITIONS_GET')
                    positions = mt5.positions_get()
                    if positions is None:
                        positions = ()
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
                    log('Received command ORDERS_GET_SYMBOL')
                    symbol = _rl()
                    orders = mt5.orders_get(symbol = symbol)
                    sendOrders(orders)
                elif line == 'ORDERS_GET_TICKET':
                    log('Received command ORDERS_GET_TICKET')
                    ticket = _rl()
                    orders = mt5.orders_get(ticket = ticket)
                    sendOrders(orders)
                elif line == 'ORDERS_GET':
                    log('Received command ORDERS_GET')
                    orders = mt5.orders_get()
                    sendOrders(orders)

                elif line == 'SYMBOLS_GET':
                    symbols = mt5.symbols_get()
                    send(len(symbols), 'Length')
                    log(symbols)
                    for xs in symbols:
                        sendSymbol(xs)

                elif line == 'SYMBOLS_GET_GROUP':
                    group = _rl()
                    symbols = mt5.symbols_get(group)
                    send(len(symbols), 'Length')
                    log(symbols)
                    for xs in symbols:
                        sendSymbol(xs)

                elif line == 'SYMBOL_SELECT':
                    symbol = str(_rl())
                    log("Py Symbol: '" + str(symbol) + "'")
                    result = mt5.symbol_select(symbol, True)
                    send(log(result))
                elif line == 'SYMBOL_INFO':
                    symbol = _rl()
                    result = mt5.symbol_info(symbol)
                    sendSymbol(result)
                elif line == 'SYMBOL_INFO_TICK':
                    symbol = _rl()
                    log(f'Received command SYMBOL_INFO_TICK for symbol: {symbol}')
                    try:
                        mt5.symbol_select(symbol, True)
                        tick = mt5.symbol_info_tick(symbol)
                        if tick is None:
                            error_info = mt5.last_error()
                            send(f'error:No tick data available for {symbol}, MT5 error: {error_info}')
                        else:
                            send('success')
                            send(tick.bid, 'bid')
                            send(tick.ask, 'ask')
                            send(tick.last, 'last')
                            send(tick.volume, 'volume')
                            send(tick.time, 'time')
                            send(tick.time_msc, 'time_msc')
                            send(tick.flags, 'flags')
                            send(tick.volume_real, 'volume_real')
                    except Exception as e:
                        send(f'error:Exception in SYMBOL_INFO_TICK: {str(e)}')
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
                    log('Received command ORDER_CANCEL')
                    ticket = int(_rl())
                    cancel_request = {
                        'action': mt5.TRADE_ACTION_REMOVE,
                        'order': ticket,
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
                    symbol = _rl()
                    timeframe = int(_rl())
                    date_from_str = _rl()
                    date_to_str = _rl()
                    log(f'Received command COPY_RATES_RANGE for symbol: {symbol}')
                    try:
                        date_from = datetime.strptime(date_from_str, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
                        date_to   = datetime.strptime(date_to_str,   '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
                        rates = mt5.copy_rates_range(symbol, timeframe, date_from, date_to)
                        if rates is None:
                            error_info = mt5.last_error()
                            send(f'error:No rate data available for {symbol}, MT5 error: {error_info}')
                        else:
                            rates_frame = pd.DataFrame(rates)
                            rates_frame['time']=pd.to_datetime(rates_frame['time'], unit='s')
                            log('rates: ' + str(rates_frame))
                            send('success')
                            send(len(rates), 'candle_count')
                            for rate in rates:
                                send(int(rate['time']), 'time')
                                send(float(rate['open']), 'open')
                                send(float(rate['high']), 'high')
                                send(float(rate['low']), 'low')
                                send(float(rate['close']), 'close')
                                send(int(rate['tick_volume']), 'volume')
                                send(int(rate['spread']), 'spread')
                                send(float(rate['real_volume']), 'real_volume')
                    except Exception as e:
                        send(f'error:Exception in COPY_RATES_RANGE: {str(e)}')

                elif line == 'COPY_RATES_FROM':
                    symbol = _rl()
                    timeframe = int(_rl())
                    date_from_str = _rl()
                    count = int(_rl())
                    log(f'Received command COPY_RATES_FROM for symbol: {symbol}')
                    try:
                        date_from = datetime.strptime(date_from_str, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
                        rates = mt5.copy_rates_from(symbol, timeframe, date_from, count)
                        if rates is None:
                            error_info = mt5.last_error()
                            send(f'error:No rate data available for {symbol}, MT5 error: {error_info}')
                        else:
                            rates_frame = pd.DataFrame(rates)
                            rates_frame['time']=pd.to_datetime(rates_frame['time'], unit='s')
                            log('rates: ' + str(rates_frame))
                            send('success')
                            send(len(rates), 'candle_count')
                            for rate in rates:
                                send(int(rate['time']), 'time')
                                send(float(rate['open']), 'open')
                                send(float(rate['high']), 'high')
                                send(float(rate['low']), 'low')
                                send(float(rate['close']), 'close')
                                send(int(rate['tick_volume']), 'volume')
                                send(int(rate['spread']), 'spread')
                                send(float(rate['real_volume']), 'real_volume')
                    except Exception as e:
                        send(f'error:Exception in COPY_RATES_FROM: {str(e)}')

                elif line == 'COPY_RATES_FROM_POS':
                    symbol = _rl()
                    timeframe = int(_rl())
                    start_pos = int(_rl())
                    count = int(_rl())
                    log(f'Received command COPY_RATES_FROM_POS for symbol: {symbol}')
                    try:
                        rates = mt5.copy_rates_from_pos(symbol, timeframe, start_pos, count)
                        if rates is None:
                            error_info = mt5.last_error()
                            send(f'error:No rate data available for {symbol}, MT5 error: {error_info}')
                        else:
                            rates_frame = pd.DataFrame(rates)
                            rates_frame['time']=pd.to_datetime(rates_frame['time'], unit='s')
                            log('rates: ' + str(rates_frame))
                            send('success')
                            send(len(rates), 'candle_count')
                            for rate in rates:
                                send(int(rate['time']), 'time')
                                send(float(rate['open']), 'open')
                                send(float(rate['high']), 'high')
                                send(float(rate['low']), 'low')
                                send(float(rate['close']), 'close')
                                send(int(rate['tick_volume']), 'volume')
                                send(int(rate['spread']), 'spread')
                                send(float(rate['real_volume']), 'real_volume')
                    except Exception as e:
                        send(f'error:Exception in COPY_RATES_FROM_POS: {str(e)}')

                elif line == 'COPY_TICKS_FROM':
                    symbol = _rl()
                    date_from_str = _rl()
                    count = int(_rl())
                    flags = int(_rl())
                    log(f'Received command COPY_TICKS_FROM for symbol: {symbol}')
                    try:
                        date_from = datetime.strptime(date_from_str, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
                        ticks = mt5.copy_ticks_from(symbol, date_from, count, flags)
                        if ticks is None:
                            error_info = mt5.last_error()
                            send(f'error:No tick data for {symbol}, MT5 error: {error_info}')
                        else:
                            send('success')
                            send(len(ticks), 'tick_count')
                            for tick in ticks:
                                send(int(tick['time_msc']), 'time_msc')
                                send(float(tick['bid']), 'bid')
                                send(float(tick['ask']), 'ask')
                                send(float(tick['last']), 'last')
                                send(int(tick['volume']), 'volume')
                                send(int(tick['flags']), 'flags')
                                send(float(tick['volume_real']), 'volume_real')
                    except Exception as e:
                        send(f'error:Exception in COPY_TICKS_FROM: {str(e)}')

                elif line == 'COPY_TICKS_RANGE':
                    symbol = _rl()
                    date_from_str = _rl()
                    date_to_str = _rl()
                    flags = int(_rl())
                    log(f'Received command COPY_TICKS_RANGE for symbol: {symbol}')
                    try:
                        date_from = datetime.strptime(date_from_str, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
                        date_to   = datetime.strptime(date_to_str,   '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
                        ticks = mt5.copy_ticks_range(symbol, date_from, date_to, flags)
                        if ticks is None:
                            error_info = mt5.last_error()
                            send(f'error:No tick data for {symbol}, MT5 error: {error_info}')
                        else:
                            send('success')
                            send(len(ticks), 'tick_count')
                            for tick in ticks:
                                send(int(tick['time_msc']), 'time_msc')
                                send(float(tick['bid']), 'bid')
                                send(float(tick['ask']), 'ask')
                                send(float(tick['last']), 'last')
                                send(int(tick['volume']), 'volume')
                                send(int(tick['flags']), 'flags')
                                send(float(tick['volume_real']), 'volume_real')
                    except Exception as e:
                        send(f'error:Exception in COPY_TICKS_RANGE: {str(e)}')

                elif line == 'ERROR':
                    formatString = _rl()
                    send(formatString.format(account, mt5.last_error()))
                elif line == 'QUIT':
                    # Disconnect this client only; daemon keeps running.
                    send('Ciao!')
                    break
                elif line == 'SHUTDOWN':
                    # Shut down the daemon entirely (sent by the owning process).
                    mt5.shutdown()
                    send('Shutdown!')
                    os._exit(0)
                else:
                    send('Unknown command: ' + line)
    except EOFError:
        pass
    except Exception as e:
        if DEBUG:
            sys.stderr.write('Client error: ' + str(e) + '\n')
            sys.stderr.flush()
    finally:
        try: _tlocal.fin.close()
        except: pass
        try: conn.close()
        except: pass


# Initialize MT5 once at daemon start.
mt5.initialize()

# Remove stale socket file if present.
if os.path.exists(SOCKET_PATH):
    os.remove(SOCKET_PATH)

_srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
_srv.bind(SOCKET_PATH)
_srv.listen(5)

while True:
    conn, _ = _srv.accept()
    t = threading.Thread(target=_handle_client, args=(conn,), daemon=True)
    t.start()
