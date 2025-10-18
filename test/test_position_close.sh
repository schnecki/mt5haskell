#!/bin/bash
#
# Test script for MT5 REST API Bridge - Close Position
# Tests position_close action
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

echo -e "${BLUE}=== MT5 REST API Bridge - Close Position Test ===${NC}\n"

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Get ticket from command line or use default
TICKET=${1:-128321707}

echo -e "${YELLOW}NOTE: This test requires an open position with ticket: $TICKET${NC}"
echo -e "${YELLOW}Usage: $0 [ticket_number]${NC}\n"

# Clean up old response
rm -f "$RESPONSE_FILE"

# Create request
echo -e "${BLUE}Requesting to close position $TICKET...${NC}"
cat > "$REQUEST_FILE" << EOF
{
  "action": "position_close",
  "data": {
    "ticket": $TICKET
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
    DEAL=$(cat "$RESPONSE_FILE" | jq -r '.deal // "N/A"')
    ORDER=$(cat "$RESPONSE_FILE" | jq -r '.order // "N/A"')
    VOLUME=$(cat "$RESPONSE_FILE" | jq -r '.volume // "N/A"')
    PRICE=$(cat "$RESPONSE_FILE" | jq -r '.price // "N/A"')

    echo ""
    echo -e "${GREEN}✓ Position closed successfully!${NC}"
    echo -e "${BLUE}Return Code:${NC} $RETCODE"
    echo -e "${BLUE}Deal:${NC} $DEAL"
    echo -e "${BLUE}Order:${NC} $ORDER"
    echo -e "${BLUE}Volume:${NC} $VOLUME"
    echo -e "${BLUE}Price:${NC} $PRICE"
else
    ERROR_CODE=$(cat "$RESPONSE_FILE" | jq -r '.error_code // "N/A"')
    ERROR_MSG=$(cat "$RESPONSE_FILE" | jq -r '.error_message // "Unknown error"')

    echo ""
    echo -e "${RED}✗ Position close failed${NC}"
    echo -e "${RED}Error Code:${NC} $ERROR_CODE"
    echo -e "${RED}Error Message:${NC} $ERROR_MSG"
    exit 1
fi

echo -e "\n${GREEN}=== Test completed successfully ===${NC}"
