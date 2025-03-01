
import json
import pickle
import sys
import os
from datetime import datetime
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

# request connection status and parameters
# print(mt5.terminal_info())
# get data on MetaTrader 5 version
# print(mt5.version())


def send(x, info = ""):
    with os.fdopen(sys.stdout.fileno(), "wb", closefd = False) as stdout:
        # if info != "":
        sys.stderr.write("Info: " + str(info) + ". Value: " + str(x) + " [Class: " + x.__class__.__name__ + "]\n")
        sys.stderr.flush()
        pickle.dump(x, stdout, protocol = 2)
        stdout.write(b"\n")
        stdout.flush()

account  = ""
password = ""
server   = "localhost"

def log(x):
    sys.stderr.write(">> " + str(x) + "\n")
    sys.stderr.flush()
    return(x)

for line in sys.stdin:
    sys.stderr.write("Py Input: " + str(line) + "\n")
    sys.stderr.flush()
    line = line.rstrip().upper()
    if line == 'INITIALIZE':
        send(log(mt5.initialize()))
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
            send(log(xs['login']))              # 173941
            send(log(xs['trade_mode']))         # 2
            send(log(xs['leverage']))           # 100
            send(log(xs['limit_orders']))       # 0
            send(log(xs['margin_so_mode']))     # 0
            send(log(xs['trade_allowed']))      # False
            send(log(xs['trade_expert']))       # True
            send(log(xs['margin_mode']))        # 2
            send(log(xs['currency_digits']))    # 2
            send(log(xs['fifo_close']))         # False
            send(log(xs['balance']))            # 330.43
            send(log(xs['credit']))             # 0.0
            send(log(xs['profit']))             # 29.61
            # send(log(xs['equity']))             # 360.04
            send(log(xs['margin']))             # 198.45
            send(log(xs['margin_free']))        # 161.59
            send(log(xs['margin_level']))       # 181.4260519022424
            send(log(xs['margin_so_call']))     # 100.0
            send(log(xs['margin_so_so']))       # 50.0
            send(log(xs['margin_initial']))     # 0.0
            send(log(xs['margin_maintenance'])) # 0.0
            send(log(xs['assets']))             # 0.0
            send(log(xs['liabilities']))        # 0.0
            send(log(xs['commission_blocked'])) # 0.0
            send(log(xs['name']))               # 'EU/M173941/EUR'
            send(log(xs['server']))             # 'OANDATMS-MT5'
            send(log(xs['currency']))           # 'EUR'
            send(log(xs['company']))            # 'OANDA TMS BrokersS.A.'


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
