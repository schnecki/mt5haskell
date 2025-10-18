#!/bin/bash
#
# Test script for MT5 REST API Bridge - Get Current Price
# Tests price_get action for specified symbol
#

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Configuration
SYMBOL="${1:-EURUSD.pro}"  # Default to EURUSD.pro if no argument provided

# MT5 Files directory
MT5_FILES_DIR="$HOME/.wine/drive_c/users/schnecki/AppData/Roaming/MetaQuotes/Terminal/Common/Files"
REQUEST_FILE="$MT5_FILES_DIR/mt5_api_request.json"
RESPONSE_FILE="$MT5_FILES_DIR/mt5_api_response.json"

echo -e "${BLUE}=== MT5 REST API Bridge - Price Get Test ===${NC}\n"
echo "Symbol: $SYMBOL"
echo ""

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Clean up old response
rm -f "$RESPONSE_FILE"

# Create request
echo -e "${BLUE}Requesting current price for $SYMBOL...${NC}"
cat > "$REQUEST_FILE" << EOF
{
  "action": "price_get",
  "data": {
    "symbol": "$SYMBOL"
  }
}
EOF

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
echo -e "${BLUE}=== Price Information ===${NC}"
cat "$RESPONSE_FILE"
echo ""

# Parse and display key info
if grep -q '"success"\s*:\s*true' "$RESPONSE_FILE"; then
    echo -e "${GREEN}✓ REQUEST SUCCESSFUL${NC}\n"

    # Extract values
    BID=$(grep -o '"bid"[[:space:]]*:[[:space:]]*[0-9.]*' "$RESPONSE_FILE" | grep -o '[0-9.]*$')
    ASK=$(grep -o '"ask"[[:space:]]*:[[:space:]]*[0-9.]*' "$RESPONSE_FILE" | grep -o '[0-9.]*$')

    echo -e "${YELLOW}Current Prices for $SYMBOL:${NC}"
    [ -n "$BID" ] && echo "  Bid (Sell): $BID"
    [ -n "$ASK" ] && echo "  Ask (Buy):  $ASK"

    if [ -n "$BID" ] && [ -n "$ASK" ]; then
        SPREAD=$(echo "$ASK - $BID" | bc -l)
        echo "  Spread:     $SPREAD"
    fi
else
    echo -e "${RED}✗ REQUEST FAILED${NC}"
    ERROR=$(grep -o '"error_message"[[:space:]]*:[[:space:]]*"[^"]*"' "$RESPONSE_FILE" | sed 's/.*: "//;s/".*//')
    [ -n "$ERROR" ] && echo "Error: $ERROR"
fi

echo -e "\n${BLUE}=== Test Complete ===${NC}\n"
echo "Usage: $0 [SYMBOL]"
echo "Example: $0 GBPUSD"
echo ""

# Cleanup
read -p "Remove request/response files? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -f "$REQUEST_FILE" "$RESPONSE_FILE"
    echo "Files removed."
fi
