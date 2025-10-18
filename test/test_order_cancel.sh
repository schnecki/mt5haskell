#!/bin/bash
#
# Test script for MT5 REST API Bridge - Cancel Order
# Tests order_cancel action
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

echo -e "${BLUE}=== MT5 REST API Bridge - Cancel Order Test ===${NC}\n"

# Usage
if [ "$1" == "-h" ] || [ "$1" == "--help" ] || [ -z "$1" ]; then
    echo "Usage: $0 <ORDER_TICKET>"
    echo ""
    echo "Examples:"
    echo "  $0 123456789           # Cancel order with ticket 123456789"
    echo ""
    echo "Note:"
    echo "  • Only pending orders can be cancelled"
    echo "  • Market orders are positions and must be closed, not cancelled"
    echo "  • Use test_orders_get.sh to find pending order tickets"
    echo ""
    echo "To test this script:"
    echo "  1. Create a pending order (buy/sell limit or stop)"
    echo "  2. Run test_orders_get.sh to get the ticket number"
    echo "  3. Run this script with the ticket number"
    exit 0
fi

ORDER_TICKET="$1"

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Clean up old response
rm -f "$RESPONSE_FILE"

# Create request
echo -e "${BLUE}Cancelling order...${NC}"
echo "  Order Ticket: $ORDER_TICKET"
cat > "$REQUEST_FILE" << EOF
{
  "action": "order_cancel",
  "data": {
    "order": $ORDER_TICKET
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
echo -e "${BLUE}=== Cancellation Result ===${NC}"
cat "$RESPONSE_FILE"
echo ""

# Parse and display summary
if grep -q '"success"\s*:\s*true' "$RESPONSE_FILE"; then
    echo -e "${GREEN}✓ ORDER CANCELLED SUCCESSFULLY${NC}\n"
    
    # Extract retcode
    RETCODE=$(grep -o '"retcode"[[:space:]]*:[[:space:]]*[0-9]*' "$RESPONSE_FILE" | grep -o '[0-9]*$' || echo "N/A")
    
    echo -e "${YELLOW}Cancellation Details:${NC}"
    echo "  Return Code: $RETCODE"
    
    # Return code descriptions
    case $RETCODE in
        10008)
            echo "    ✓ Order placed (deletion request accepted)"
            ;;
        10009)
            echo "    ✓ Request completed"
            ;;
        10013)
            echo "    ✗ Invalid request"
            ;;
        10016)
            echo "    ✗ Invalid order"
            ;;
        10017)
            echo "    ✗ Trading disabled (enable AutoTrading in MT5)"
            ;;
        10019)
            echo "    ✗ No money"
            ;;
        *)
            echo "    (See MT5 documentation for return code details)"
            ;;
    esac
    
    # Extract order number if present
    ORDER=$(grep -o '"order"[[:space:]]*:[[:space:]]*[0-9]*' "$RESPONSE_FILE" | grep -o '[0-9]*$' || echo "N/A")
    [ "$ORDER" != "N/A" ] && echo "  Order Ticket: $ORDER"
    
else
    echo -e "${RED}✗ ORDER CANCELLATION FAILED${NC}\n"
    
    echo -e "${YELLOW}Common reasons for failure:${NC}"
    echo "  • Order ticket does not exist"
    echo "  • Order already executed"
    echo "  • Order already cancelled"
    echo "  • Trying to cancel a market position (use close position instead)"
    echo "  • Trading is disabled (enable AutoTrading in MT5)"
fi

echo -e "\n${BLUE}=== Test Complete ===${NC}\n"

# Cleanup
read -p "Remove request/response files? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -f "$REQUEST_FILE" "$RESPONSE_FILE"
    echo "Files removed."
fi
