#!/usr/bin/env python3
"""
MT5 REST API Server - HTTP to File Bridge
Provides REST API interface that communicates with MT5 Expert Advisor via files.
"""

from flask import Flask, request, jsonify
import json
import os
import time
from pathlib import Path

app = Flask(__name__)

# Configuration
MT5_COMMON_DATA_PATH = os.path.expanduser(
    "~/.wine/drive_c/users/*/AppData/Roaming/MetaQuotes/Terminal/*/MQL5/Files"
)
REQUEST_FILE = "mt5_api_request.json"
RESPONSE_FILE = "mt5_api_response.json"
TIMEOUT_SECONDS = 10
CHECK_INTERVAL = 0.1

def find_mt5_files_directory():
    """Find MT5 Files directory using glob pattern"""
    import glob
    paths = glob.glob(MT5_COMMON_DATA_PATH)
    if paths:
        return paths[0]
    # Fallback to common path
    return os.path.expanduser("~/.wine/drive_c/Program Files/MetaTrader 5/MQL5/Files")

FILES_DIR = find_mt5_files_directory()
REQUEST_PATH = os.path.join(FILES_DIR, REQUEST_FILE)
RESPONSE_PATH = os.path.join(FILES_DIR, RESPONSE_FILE)

print(f"MT5 Files directory: {FILES_DIR}")
print(f"Request file: {REQUEST_PATH}")
print(f"Response file: {RESPONSE_PATH}")

def send_request_to_mt5(endpoint, data=None):
    """Send request to MT5 EA via file and wait for response"""

    # Prepare request
    request_data = {
        "endpoint": endpoint,
        "data": data or {}
    }

    # Write request file
    try:
        with open(REQUEST_PATH, 'w') as f:
            json.dump(request_data, f)
    except Exception as e:
        return {"success": False, "error_message": f"Failed to write request: {str(e)}"}

    # Clear old response if exists
    if os.path.exists(RESPONSE_PATH):
        try:
            os.remove(RESPONSE_PATH)
        except:
            pass

    # Wait for response
    start_time = time.time()
    while time.time() - start_time < TIMEOUT_SECONDS:
        if os.path.exists(RESPONSE_PATH) and os.path.getsize(RESPONSE_PATH) > 0:
            try:
                with open(RESPONSE_PATH, 'r') as f:
                    response_content = f.read()
                    if response_content:
                        response = json.loads(response_content)
                        # Clear request file
                        try:
                            os.remove(REQUEST_PATH)
                        except:
                            pass
                        return response
            except json.JSONDecodeError:
                # Response file not complete yet
                pass
            except Exception as e:
                return {"success": False, "error_message": f"Failed to read response: {str(e)}"}

        time.sleep(CHECK_INTERVAL)

    # Timeout
    return {"success": False, "error_message": "Request timeout - MT5 EA may not be running"}

@app.route('/api/health', methods=['GET'])
def health_check():
    """Health check endpoint"""
    return jsonify({"status": "ok", "message": "MT5 REST API Server is running"})

@app.route('/api/order/send', methods=['POST'])
def order_send():
    """Send order to MT5"""
    data = request.json
    response = send_request_to_mt5("/api/order/send", data)
    return jsonify(response)

@app.route('/api/order/check', methods=['POST'])
def order_check():
    """Check order validity"""
    data = request.json
    response = send_request_to_mt5("/api/order/check", data)
    return jsonify(response)

@app.route('/api/order/cancel', methods=['POST'])
def order_cancel():
    """Cancel pending order"""
    data = request.json
    response = send_request_to_mt5("/api/order/cancel", data)
    return jsonify(response)

@app.route('/api/positions', methods=['GET'])
def positions_get():
    """Get all open positions"""
    response = send_request_to_mt5("/api/positions")
    return jsonify(response)

@app.route('/api/orders', methods=['GET'])
def orders_get():
    """Get all pending orders"""
    response = send_request_to_mt5("/api/orders")
    return jsonify(response)

@app.route('/api/account', methods=['GET'])
def account_info():
    """Get account information"""
    response = send_request_to_mt5("/api/account")
    return jsonify(response)

@app.route('/api/price/<symbol>', methods=['GET'])
def price_get(symbol):
    """Get current price for symbol"""
    response = send_request_to_mt5(f"/api/price/{symbol}", {"symbol": symbol})
    return jsonify(response)

if __name__ == '__main__':
    # Ensure Files directory exists
    os.makedirs(FILES_DIR, exist_ok=True)

    print("\n" + "="*60)
    print("MT5 REST API Server Starting...")
    print("="*60)
    print(f"Server will listen on: http://localhost:8085")
    print(f"MT5 Files directory: {FILES_DIR}")
    print("\nMake sure MT5RestAPIBridge.mq5 EA is running in MetaTrader 5!")
    print("="*60 + "\n")

    app.run(host='0.0.0.0', port=8085, debug=True)
