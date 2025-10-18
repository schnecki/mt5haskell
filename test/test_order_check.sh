#!/bin/bash
#
# Test script for MT5 REST API Bridge - Check Order
# Tests order_check action (validates order without executing)
#

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# MT5 Files directory
MT5_FILES_DIR="$HOME/.wine/drive_c/users/schnecki/AppData/Roaming/MetaQuotes/Terminal/Common/Files"
REQUEST_FILE="$MT5_FILES_DIR/mt5_api_request.json"
RESPONSE_FILE="$MT5_FILES_DIR/mt5_api_response.json"

# Default order parameters
SYMBOL="${1:-EURUSD.pro}"
VOLUME="${2:-0.01}"
ORDER_TYPE="${3:-0}"  # 0=BUY, 1=SELL
PRICE="${4:-}"  # Optional price (for limit/stop orders)

echo -e "${BLUE}=== MT5 REST API Bridge - Check Order Test ===${NC}\n"

# Usage
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
    echo "Usage: $0 [SYMBOL] [VOLUME] [ORDER_TYPE] [PRICE]"
    echo ""
    echo "Examples:"
    echo "  $0                          # Check EURUSD.pro 0.01 market buy"
    echo "  $0 EURUSD.pro 0.05          # Check EURUSD.pro 0.05 market buy"
    echo "  $0 EURUSD.pro 0.01 1        # Check EURUSD.pro 0.01 market sell"
    echo "  $0 EURUSD.pro 0.02 2 1.0500 # Check buy limit at 1.0500"
    echo ""
    echo "Order types:"
    echo "  0 = Buy (market)"
    echo "  1 = Sell (market)"
    echo "  2 = Buy Limit"
    echo "  3 = Sell Limit"
    echo "  4 = Buy Stop"
    echo "  5 = Sell Stop"
    exit 0
fi

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Clean up old response
rm -f "$RESPONSE_FILE"

# Build request
echo -e "${BLUE}Checking order...${NC}"
echo "  Symbol: $SYMBOL"
echo "  Volume: $VOLUME"
echo "  Type: $ORDER_TYPE"
[ -n "$PRICE" ] && echo "  Price: $PRICE"

# Create request JSON
if [ -n "$PRICE" ]; then
    # With price
    cat > "$REQUEST_FILE" << EOF
{
  "action": "order_check",
  "data": {
    "symbol": "$SYMBOL",
    "volume": $VOLUME,
    "type": $ORDER_TYPE,
    "price": $PRICE,
    "sl": 0.0,
    "tp": 0.0,
    "comment": "Order check test"
  }
}
EOF
else
    # Market order (no price)
    cat > "$REQUEST_FILE" << EOF
{
  "action": "order_check",
  "data": {
    "symbol": "$SYMBOL",
    "volume": $VOLUME,
    "type": $ORDER_TYPE,
    "sl": 0.0,
    "tp": 0.0,
    "comment": "Order check test"
  }
}
EOF
fi

echo -e "${GREEN}✓ Request file created${NC}\n"

# Wait for response
echo -e "${BLUE}Waiting for MT5 response...${NC}"
TIMEOUT=10
ELAPSED=0

while [ $ELAPSED -lt $TIMEOUT ]; do
    if [ -f "$RESPONSE_FILE" ]; then
        echo -e "${GREEN}✓ Response received!${NC}\n"
        break
    fi
    sleep 0.5
    ELAPSED=$((ELAPSED + 1))
    echo -n "."
done

echo ""

if [ ! -f "$RESPONSE_FILE" ]; then
    echo -e "${RED}ERROR: No response received${NC}"
    exit 1
fi

# Display response
echo -e "${BLUE}=== Check Result ===${NC}"
cat "$RESPONSE_FILE"
echo ""

# Parse and display summary
if grep -q '"success"\s*:\s*true' "$RESPONSE_FILE"; then
    echo -e "${GREEN}✓ ORDER VALIDATION SUCCESSFUL${NC}\n"

    echo -e "${YELLOW}Order Check Results:${NC}"

    # Extract retcode
    RETCODE=$(grep -o '"retcode"[[:space:]]*:[[:space:]]*[0-9]*' "$RESPONSE_FILE" | grep -o '[0-9]*$' || echo "N/A")
    echo "  Return Code: $RETCODE"

    # Return code descriptions
    case $RETCODE in
        0)
            echo "    ✓ Order check passed - order is valid"
            ;;
        10013)
            echo "    ✗ Invalid request"
            ;;
        10014)
            echo "    ✗ Invalid volume"
            ;;
        10015)
            echo "    ✗ Invalid price"
            ;;
        10016)
            echo "    ✗ Invalid stops"
            ;;
        10019)
            echo "    ✗ Insufficient margin"
            ;;
        10020)
            echo "    ✗ No money"
            ;;
        *)
            echo "    (See MT5 documentation for return code details)"
            ;;
    esac

    echo ""
    echo -e "${YELLOW}Important:${NC}"
    echo "  • This is a validation check only"
    echo "  • No actual order was placed"
    echo "  • Use this to verify margin requirements before trading"

else
    echo -e "${RED}✗ ORDER VALIDATION FAILED${NC}"
fi

echo -e "\n${BLUE}=== Test Complete ===${NC}\n"

# Cleanup
read -p "Remove request/response files? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -f "$REQUEST_FILE" "$RESPONSE_FILE"
    echo "Files removed."
fi
