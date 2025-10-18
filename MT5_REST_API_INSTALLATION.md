# MT5 REST API Bridge - Installation Guide

## Overview

The MT5 REST API Bridge provides a file-based communication channel between your applications (Haskell, Bash, Python, etc.) and MetaTrader 5.

**Architecture:**
```
Your Application → JSON Request File → MT5 EA → Executes Trade
                                        ↓
Your Application ← JSON Response File ← MT5 EA
```

**No Python server required!** Your Haskell application writes directly to MT5's Files directory.

## Quick Installation (2 Minutes)

### Prerequisites

1. **MetaTrader 5** installed via Wine
2. **Expert Advisor enabled** in MT5
3. **MT5 Files directory** accessible

### Installation Steps

#### 1. Copy Expert Advisor to MT5

```bash
cd /home/schnecki/Documents/projects/mt5haskell

# Copy EA to MT5 Experts folder
cp MQL5/Experts/MT5RestAPIBridge.mq5 \
   "$HOME/.wine/drive_c/Program Files/MetaTrader 5/MQL5/Experts/"
```

#### 2. Compile EA in MT5

1. Open MetaTrader 5
2. Press `F4` to open MetaEditor
3. File → Open → Navigate to `Experts/MT5RestAPIBridge.mq5`
4. Press `F7` to compile
5. Verify: "0 error(s), 0 warning(s)"

#### 3. Enable Expert Advisor

1. In MT5, go to `Tools → Options → Expert Advisors`
2. Check these options:
   - ✅ Allow algorithmic trading
   - ✅ Allow DLL imports
3. Click OK

#### 4. Attach EA to Chart

1. In MT5, open any chart (e.g., EURUSD)
2. Navigator panel → Expert Advisors
3. Drag `MT5RestAPIBridge` onto the chart
4. In the dialog:
   - **RequestFile:** `mt5_api_request.json` (default)
   - **ResponseFile:** `mt5_api_response.json` (default)
   - **CheckIntervalMs:** `100` (default)
   - **EnableLogging:** `true` (default)
5. Check "Allow algorithmic trading"
6. Click OK

#### 5. Verify Installation

Check MT5 Experts tab (bottom panel) for:
```
MT5 REST API Bridge initializing...
MT5 REST API Bridge started successfully
Waiting for requests in: C:\Users\...\Terminal\...\MQL5\Files\mt5_api_request.json
```

## File Locations

### MT5 Files Directory

**Linux (Wine) - Common/Files Directory:**

MT5 uses a shared Common/Files directory for file operations across all terminals:

```
$HOME/.wine/drive_c/users/$USER/AppData/Roaming/MetaQuotes/Terminal/Common/Files/
```

For user `schnecki`:
```
$HOME/.wine/drive_c/users/schnecki/AppData/Roaming/MetaQuotes/Terminal/Common/Files/
```

**Request File:**
```
$HOME/.wine/drive_c/users/schnecki/AppData/Roaming/MetaQuotes/Terminal/Common/Files/mt5_api_request.json
```

**Response File:**
```
$HOME/.wine/drive_c/users/schnecki/AppData/Roaming/MetaQuotes/Terminal/Common/Files/mt5_api_response.json
```

### Expert Advisor Location

```
$HOME/.wine/drive_c/Program Files/MetaTrader 5/MQL5/Experts/MT5RestAPIBridge.mq5
```

## Testing the Bridge

### Test 1: Market Order Creation

```bash
cd /home/schnecki/Documents/projects/mt5haskell/scripts
./test_market_order.sh
```

This will:
1. Create a request file for EURUSD Buy 0.01 lots
2. Wait for MT5 to process the request
3. Display the response
4. Show order details or error message

### Test 2: Manual Testing

#### Step 1: Create Request File

```bash
# Set MT5 files directory (use your actual username)
MT5_FILES="$HOME/.wine/drive_c/users/$USER/AppData/Roaming/MetaQuotes/Terminal/Common/Files"

# Create request
cat > "$MT5_FILES/mt5_api_request.json" << 'EOF'
{
  "action": "order_send",
  "data": {
    "symbol": "EURUSD",
    "volume": 0.01,
    "type": 0,
    "price": 0.0,
    "sl": 0.0,
    "tp": 0.0,
    "comment": "Test order",
    "deviation": 20,
    "magic": 12345
  }
}
EOF
```

#### Step 2: Wait for Response (MT5 checks every 100ms)

```bash
# Wait a moment
sleep 1

# Read response
cat "$MT5_FILES/mt5_api_response.json"
```

#### Step 3: Check Response

Success response:
```json
{
  "success": true,
  "retcode": 10009,
  "retcode_description": "Request completed",
  "order": 123456789,
  "deal": 123456789,
  "volume": 0.01,
  "price": 1.08234,
  ...
}
```

Error response:
```json
{
  "success": false,
  "retcode": 10015,
  "retcode_description": "Invalid stops",
  ...
}
```

## API Reference

### Request Format

All requests use this structure:

```json
{
  "action": "ACTION_NAME",
  "data": {
    // Action-specific parameters
  }
}
```

### Available Actions

#### 1. Send Market Order

**Action:** `order_send`

**Request:**
```json
{
  "action": "order_send",
  "data": {
    "symbol": "EURUSD",           // Trading symbol
    "volume": 0.01,               // Lot size
    "type": 0,                    // 0=Buy, 1=Sell
    "price": 0.0,                 // 0.0 for market orders
    "sl": 0.0,                    // Stop loss (0.0 = none)
    "tp": 0.0,                    // Take profit (0.0 = none)
    "comment": "My order",        // Order comment
    "deviation": 20,              // Allowed price deviation in points
    "magic": 12345                // Magic number (optional)
  }
}
```

**Response:**
```json
{
  "success": true,
  "retcode": 10009,               // MT5 return code
  "retcode_description": "...",   // Description
  "order": 123456789,             // Order ticket
  "deal": 123456789,              // Deal ticket
  "volume": 0.01,                 // Executed volume
  "price": 1.08234,               // Execution price
  "bid": 1.08232,                 // Current bid
  "ask": 1.08236                  // Current ask
}
```

#### 2. Get Open Positions

**Action:** `positions_get`

**Request:**
```json
{
  "action": "positions_get",
  "data": {}
}
```

**Response:**
```json
{
  "success": true,
  "count": 2,
  "positions": [
    {
      "ticket": 123456789,
      "symbol": "EURUSD",
      "type": 0,                  // 0=Buy, 1=Sell
      "volume": 0.01,
      "price_open": 1.08234,
      "price_current": 1.08456,
      "sl": 1.07500,
      "tp": 1.09000,
      "profit": 2.22,
      "swap": 0.0,
      "commission": 0.0,
      "magic": 12345,
      "comment": "My position"
    },
    ...
  ]
}
```

#### 3. Get Pending Orders

**Action:** `orders_get`

**Request:**
```json
{
  "action": "orders_get",
  "data": {}
}
```

**Response:**
```json
{
  "success": true,
  "count": 1,
  "orders": [
    {
      "ticket": 987654321,
      "symbol": "EURUSD",
      "type": 2,                  // 2=BuyLimit, 3=SellLimit, etc.
      "volume_initial": 0.01,
      "volume_current": 0.01,
      "price_open": 1.07000,
      "sl": 1.06500,
      "tp": 1.08000,
      "magic": 12345,
      "comment": "Pending order",
      "state": 1                  // Order state
    },
    ...
  ]
}
```

#### 4. Cancel Pending Order

**Action:** `order_cancel`

**Request:**
```json
{
  "action": "order_cancel",
  "data": {
    "ticket": 987654321          // Order ticket to cancel
  }
}
```

**Response:**
```json
{
  "success": true,
  "retcode": 10009,
  "retcode_description": "Request completed"
}
```

#### 5. Get Account Information

**Action:** `account_info`

**Request:**
```json
{
  "action": "account_info",
  "data": {}
}
```

**Response:**
```json
{
  "success": true,
  "login": 12345678,
  "balance": 10000.00,
  "equity": 10025.50,
  "profit": 25.50,
  "margin": 50.00,
  "margin_free": 9975.50,
  "margin_level": 20051.00,
  "currency": "USD",
  "name": "Demo Account",
  "server": "MetaQuotes-Demo",
  "leverage": 100
}
```

## Order Types Reference

| Type | Value | Description |
|------|-------|-------------|
| ORDER_TYPE_BUY | 0 | Buy market order |
| ORDER_TYPE_SELL | 1 | Sell market order |
| ORDER_TYPE_BUY_LIMIT | 2 | Buy limit pending order |
| ORDER_TYPE_SELL_LIMIT | 3 | Sell limit pending order |
| ORDER_TYPE_BUY_STOP | 4 | Buy stop pending order |
| ORDER_TYPE_SELL_STOP | 5 | Sell stop pending order |

## Return Codes Reference

| Code | Constant | Description |
|------|----------|-------------|
| 10009 | TRADE_RETCODE_DONE | Request completed |
| 10004 | TRADE_RETCODE_REQUOTE | Requote |
| 10006 | TRADE_RETCODE_REJECT | Request rejected |
| 10007 | TRADE_RETCODE_CANCEL | Request canceled |
| 10008 | TRADE_RETCODE_PLACED | Order placed |
| 10013 | TRADE_RETCODE_INVALID_VOLUME | Invalid volume |
| 10014 | TRADE_RETCODE_INVALID_PRICE | Invalid price |
| 10015 | TRADE_RETCODE_INVALID_STOPS | Invalid stops |
| 10016 | TRADE_RETCODE_TRADE_DISABLED | Trade disabled |
| 10018 | TRADE_RETCODE_MARKET_CLOSED | Market closed |
| 10019 | TRADE_RETCODE_NO_MONEY | Not enough money |

Full list: https://www.mql5.com/en/docs/constants/errorswarnings/enum_trade_return_codes

## Troubleshooting

### EA Not Starting

**Check:**
1. `Tools → Options → Expert Advisors → Allow algorithmic trading` is checked
2. EA is attached to a chart (smile icon in top-right corner)
3. MT5 Experts tab shows "MT5 REST API Bridge started successfully"

**Fix:**
- Right-click chart → Expert Advisors → Enable
- Or press `Ctrl+E`

### No Response File Created

**Possible Causes:**
1. EA not running (check Experts tab)
2. Request file format invalid (check JSON syntax)
3. File permissions issue

**Debug:**
- Check MT5 Experts tab for error messages
- Enable logging: EA Settings → EnableLogging = true
- Verify request file location

### Order Execution Fails

**Common Reasons:**
- Market closed (weekends)
- Insufficient margin
- Invalid symbol name
- Volume too small/large
- Invalid price levels

**Check Response:**
Look at `retcode` and `retcode_description` in response file for details.

### File Permission Issues

If MT5 can't read/write files:

```bash
# Ensure MT5 Files directory exists
mkdir -p "$HOME/.wine/drive_c/users/$USER/AppData/Roaming/MetaQuotes/Terminal/Common/Files"

# Set permissions
chmod -R 755 "$HOME/.wine/drive_c/users/$USER/AppData/Roaming/MetaQuotes/Terminal/Common/Files"
```

## Haskell Integration Example

### File Path Helper

```haskell
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson

```haskell
mt5FilesDir :: IO FilePath
mt5FilesDir = do
  home <- getHomeDirectory
  user <- getEnv "USER"
  return $ home </> ".wine/drive_c/users" </> user </> "AppData/Roaming/MetaQuotes/Terminal/Common/Files"

requestFilePath :: IO FilePath
requestFilePath = (</> "mt5_api_request.json") <$> mt5FilesDir

responseFilePath :: IO FilePath
responseFilePath = (</> "mt5_api_response.json") <$> mt5FilesDir
```

### Send Order Function

```haskell
sendMarketOrder :: String -> Double -> IO (Maybe OrderResponse)
sendMarketOrder symbol volume = do
  let request = object
        [ "action" .= ("order_send" :: Text)
        , "data" .= object
            [ "symbol" .= symbol
            , "volume" .= volume
            , "type" .= (0 :: Int)  -- Buy
            , "price" .= (0.0 :: Double)
            , "sl" .= (0.0 :: Double)
            , "tp" .= (0.0 :: Double)
            , "comment" .= ("Haskell order" :: Text)
            , "deviation" .= (20 :: Int)
            , "magic" .= (12345 :: Int)
            ]
        ]
  
  -- Write request
  reqPath <- requestFilePath
  BSL.writeFile reqPath (encode request)
  
  -- Wait for response (max 5 seconds)
  respPath <- responseFilePath
  waitForResponse respPath 50  -- 50 * 100ms = 5s
  
  where
    waitForResponse path 0 = return Nothing
    waitForResponse path n = do
      exists <- doesFileExist path
      if exists
        then do
          content <- BSL.readFile path
          return $ decode content
        else do
          threadDelay 100000  -- 100ms
          waitForResponse path (n - 1)
```

## Performance Notes

- **Latency:** ~5-15ms from file write to MT5 processing
- **Throughput:** Suitable for trading (not HFT)
- **Checking Interval:** EA checks every 100ms by default
- **File I/O:** Very reliable on local filesystem

## Next Steps

1. ✅ **Installed:** MT5 REST API Bridge EA
2. ✅ **Tested:** Order creation works
3. **Integrate:** Add file communication to your Haskell code
4. **Implement:** MT5Communication typeclass in your project
5. **Deploy:** Use in production trading

For Haskell integration details, see:
- `plans/TASK_SUMMARY_DETAILED_PHASE_PLAN.md`

## Support

**Issues?**
- Check MT5 Experts tab for error messages
- Enable logging in EA settings
- Test with provided bash script first
- Review API reference for correct request format

**Questions?**
- See troubleshooting section above
- Check MT5 return codes reference
- Review example requests in this guide
