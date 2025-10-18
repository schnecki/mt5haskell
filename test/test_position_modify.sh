#!/bin/bash
#
# Test script for MT5 REST API Bridge - Modify Position
# Tests position_modify action
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

echo -e "${BLUE}=== MT5 REST API Bridge - Modify Position Test ===${NC}\n"

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Get parameters from command line or use defaults
TICKET=${1:-123456789}
SL=${2:-0.0}
TP=${3:-0.0}

echo -e "${YELLOW}NOTE: This test requires an open position with ticket: $TICKET${NC}"
echo -e "${YELLOW}Usage: $0 [ticket_number] [stop_loss] [take_profit]${NC}"
echo -e "${YELLOW}Example: $0 123456789 1.0900 1.1100${NC}\n"

# Clean up old response
rm -f "$RESPONSE_FILE"

# Create request
echo -e "${BLUE}Requesting to modify position $TICKET (SL: $SL, TP: $TP)...${NC}"
cat > "$REQUEST_FILE" << EOF
{
  "action": "position_modify",
  "data": {
    "ticket": $TICKET,
    "sl": $SL,
    "tp": $TP
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
    echo -e "${RED}ERROR: No response received from MT5 within ${TIMEOUT} seconds${NC}"
    echo -e "${YELLOW}Make sure the EA is running and attached to a chart${NC}"
    exit 1
fi

# Display response
echo -e "${BLUE}=== MT5 Response ===${NC}"
cat "$RESPONSE_FILE" | jq '.'

# Parse response
SUCCESS=$(cat "$RESPONSE_FILE" | jq -r '.success // false')

if [ "$SUCCESS" = "true" ]; then
    RETCODE=$(cat "$RESPONSE_FILE" | jq -r '.retcode // "N/A"')
    ORDER=$(cat "$RESPONSE_FILE" | jq -r '.order // "N/A"')
    SL_RESULT=$(cat "$RESPONSE_FILE" | jq -r '.sl // "N/A"')
    TP_RESULT=$(cat "$RESPONSE_FILE" | jq -r '.tp // "N/A"')
    
    echo ""
    echo -e "${GREEN}✓ Position modified successfully!${NC}"
    echo -e "${BLUE}Return Code:${NC} $RETCODE"
    echo -e "${BLUE}Order:${NC} $ORDER"
    echo -e "${BLUE}Stop Loss:${NC} $SL_RESULT"
    echo -e "${BLUE}Take Profit:${NC} $TP_RESULT"
else
    ERROR_CODE=$(cat "$RESPONSE_FILE" | jq -r '.error_code // "N/A"')
    ERROR_MSG=$(cat "$RESPONSE_FILE" | jq -r '.error_message // "Unknown error"')
    
    echo ""
    echo -e "${RED}✗ Position modify failed${NC}"
    echo -e "${RED}Error Code:${NC} $ERROR_CODE"
    echo -e "${RED}Error Message:${NC} $ERROR_MSG"
    exit 1
fi

echo -e "\n${GREEN}=== Test completed successfully ===${NC}"
