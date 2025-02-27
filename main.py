

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
if not mt5.initialize():
    print("initialize() failed")
    mt5.shutdown()

# request connection status and parameters
# print(mt5.terminal_info())
# get data on MetaTrader 5 version
# print(mt5.version())


def send(x, info = ""):
    with os.fdopen(sys.stdout.fileno(), "wb", closefd = False) as stdout:
        if info != "":
            sys.stderr.write("Info: " + str(info) + ". Value: " + str(x) + " [Class: " + x.__class__.__name__ + "]\n")
            sys.stderr.flush()
        pickle.dump(x, stdout, protocol = 2)
        stdout.write(b"\n")
        stdout.flush()


for line in sys.stdin:
    sys.stderr.write("Py Input: " + str(line) + "\n")
    sys.stderr.flush()
    line = line.rstrip().upper()
    if line == 'OBSERVATION_SPACE_SAMPLE':
        send("TODO")
    elif line == 'QUIT':
        mt5.shutdown()
        send("Ciao!")
        break
    else:
        send("Unknown command: " + line)

    sys.stdout.flush()


mt5.shutdown()
