#!/bin/bash
#
# Test script for MT5 REST API Bridge - Get Pending Orders
# Tests orders_get action
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

echo -e "${BLUE}=== MT5 REST API Bridge - Get Pending Orders Test ===${NC}\n"

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Clean up old response
rm -f "$RESPONSE_FILE"

# Create request
echo -e "${BLUE}Requesting pending orders...${NC}"
cat > "$REQUEST_FILE" << 'EOF'
{
  "action": "orders_get",
  "data": {}
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
echo -e "${BLUE}=== Pending Orders ===${NC}"
cat "$RESPONSE_FILE"
echo ""

# Parse and display summary
if grep -q '"success"\s*:\s*true' "$RESPONSE_FILE"; then
    echo -e "${GREEN}✓ REQUEST SUCCESSFUL${NC}\n"
    
    # Count orders
    COUNT=$(grep -o '"count"[[:space:]]*:[[:space:]]*[0-9]*' "$RESPONSE_FILE" | grep -o '[0-9]*$')
    
    echo -e "${YELLOW}Summary:${NC}"
    echo "  Pending Orders: $COUNT"
    
    if [ "$COUNT" -gt 0 ]; then
        echo ""
        echo -e "${YELLOW}Note:${NC} See full JSON output above for order details"
        echo "Each order includes: ticket, symbol, type, volume, price, sl, tp, state, etc."
        echo ""
        echo "Order types:"
        echo "  2 = Buy Limit"
        echo "  3 = Sell Limit"
        echo "  4 = Buy Stop"
        echo "  5 = Sell Stop"
        echo "  6 = Buy Stop Limit"
        echo "  7 = Sell Stop Limit"
    else
        echo ""
        echo "  No pending orders found."
        echo "  (Pending orders are limit/stop orders, not market orders)"
    fi
else
    echo -e "${RED}✗ REQUEST FAILED${NC}"
fi

echo -e "\n${BLUE}=== Test Complete ===${NC}\n"

# Cleanup
read -p "Remove request/response files? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -f "$REQUEST_FILE" "$RESPONSE_FILE"
    echo "Files removed."
fi
