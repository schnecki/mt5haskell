#!/bin/bash
#
# Test script for MT5 REST API Bridge - Market Order Creation
# Tests EURUSD Buy Market Order with 0.01 lots
#

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# MT5 Files directory (Common/Files for shared file access across terminals)
MT5_FILES_DIR="$HOME/.wine/drive_c/users/schnecki/AppData/Roaming/MetaQuotes/Terminal/Common/Files"
REQUEST_FILE="$MT5_FILES_DIR/mt5_api_request.json"
RESPONSE_FILE="$MT5_FILES_DIR/mt5_api_response.json"

echo -e "${BLUE}=== MT5 REST API Bridge - Order Creation Test ===${NC}\n"

# Check if MT5 Files directory exists
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found:${NC}"
    echo "$MT5_FILES_DIR"
    echo ""
    echo "Please ensure:"
    echo "  1. MetaTrader 5 is installed via Wine"
    echo "  2. MT5 has been run at least once"
    exit 1
fi

# Clean up old response file
if [ -f "$RESPONSE_FILE" ]; then
    echo "Removing old response file..."
    rm -f "$RESPONSE_FILE"
fi

# Create request JSON for market buy order
echo -e "${BLUE}Creating market order request...${NC}"
cat > "$REQUEST_FILE" << 'EOF'
{
  "action": "order_send",
  "data": {
    "symbol": "EURUSD.pro",
    "volume": 0.01,
    "type": 0,
    "price": 0.0,
    "sl": 0.0,
    "tp": 0.0,
    "comment": "Test market order from Haskell/Bash",
    "deviation": 20,
    "magic": 12345
  }
}
EOF

echo -e "${GREEN}✓ Request file created${NC}"
echo ""
cat "$REQUEST_FILE"
echo ""

# Wait for MT5 EA to process request (checks every 100ms by default)
echo -e "${BLUE}Waiting for MT5 to process request...${NC}"
TIMEOUT=10
ELAPSED=0

while [ $ELAPSED -lt $TIMEOUT ]; do
    if [ -f "$RESPONSE_FILE" ]; then
        echo -e "${GREEN}✓ Response received!${NC}"
        echo ""
        break
    fi
    sleep 0.5
    ELAPSED=$((ELAPSED + 1))
    echo -n "."
done

echo ""

# Check if response was received
if [ ! -f "$RESPONSE_FILE" ]; then
    echo -e "${RED}ERROR: No response received after ${TIMEOUT} seconds${NC}"
    echo ""
    echo "Troubleshooting:"
    echo "  1. Check if MT5RestAPIBridge EA is running in MT5"
    echo "  2. Check MT5 Experts tab for errors"
    echo "  3. Verify request file was created: $REQUEST_FILE"
    exit 1
fi

# Display response
echo -e "${BLUE}=== MT5 Response ===${NC}"
cat "$RESPONSE_FILE"
echo ""

# Parse response for success
if grep -q '"success"\s*:\s*true' "$RESPONSE_FILE"; then
    echo -e "${GREEN}✓ ORDER CREATED SUCCESSFULLY!${NC}"

    # Extract order details
    ORDER_ID=$(grep -o '"order"[[:space:]]*:[[:space:]]*[0-9]*' "$RESPONSE_FILE" | grep -o '[0-9]*')
    RETCODE=$(grep -o '"retcode"[[:space:]]*:[[:space:]]*[0-9]*' "$RESPONSE_FILE" | grep -o '[0-9]*')

    if [ -n "$ORDER_ID" ]; then
        echo "Order ID: $ORDER_ID"
    fi
    if [ -n "$RETCODE" ]; then
        echo "Return Code: $RETCODE"
    fi
else
    echo -e "${RED}✗ ORDER FAILED${NC}"

    # Extract error details
    RETCODE=$(grep -o '"retcode"[[:space:]]*:[[:space:]]*[0-9]*' "$RESPONSE_FILE" | grep -o '[0-9]*')
    ERROR_MSG=$(grep -o '"retcode_description"[[:space:]]*:[[:space:]]*"[^"]*"' "$RESPONSE_FILE" | sed 's/.*: "//;s/".*//')

    if [ -n "$RETCODE" ]; then
        echo "Return Code: $RETCODE"
    fi
    if [ -n "$ERROR_MSG" ]; then
        echo "Error: $ERROR_MSG"
    fi
fi

echo ""
echo -e "${BLUE}=== Test Complete ===${NC}"

# Cleanup
echo ""
read -p "Remove request/response files? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -f "$REQUEST_FILE" "$RESPONSE_FILE"
    echo "Files removed."
fi
