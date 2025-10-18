#!/bin/bash
#
# Test script for MT5 REST API Bridge - Symbol Select
# Tests symbol_select action
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

echo -e "${BLUE}=== MT5 REST API Bridge - Symbol Select Test ===${NC}\n"

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Get parameters from command line or use defaults
SYMBOL=${1:-"GBPUSD"}
ENABLE=${2:-"true"}

echo -e "${YELLOW}Symbol: $SYMBOL${NC}"
echo -e "${YELLOW}Enable: $ENABLE${NC}"
echo -e "${YELLOW}Usage: $0 [symbol] [true|false]${NC}\n"

# Clean up old response
rm -f "$RESPONSE_FILE"

# Create request
echo -e "${BLUE}Requesting to $([ "$ENABLE" = "true" ] && echo "enable" || echo "disable") symbol $SYMBOL...${NC}"
cat > "$REQUEST_FILE" << EOF
{
  "action": "symbol_select",
  "data": {
    "symbol": "$SYMBOL",
    "enable": $ENABLE
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
    SYMBOL_NAME=$(cat "$RESPONSE_FILE" | jq -r '.symbol // "N/A"')
    SELECTED=$(cat "$RESPONSE_FILE" | jq -r '.selected // "N/A"')
    
    echo ""
    echo -e "${GREEN}✓ Symbol select operation successful!${NC}"
    echo -e "${BLUE}Symbol:${NC} $SYMBOL_NAME"
    echo -e "${BLUE}Selected:${NC} $SELECTED"
else
    ERROR_CODE=$(cat "$RESPONSE_FILE" | jq -r '.error_code // "N/A"')
    ERROR_MSG=$(cat "$RESPONSE_FILE" | jq -r '.error_message // "Unknown error"')
    
    echo ""
    echo -e "${RED}✗ Symbol select failed${NC}"
    echo -e "${RED}Error Code:${NC} $ERROR_CODE"
    echo -e "${RED}Error Message:${NC} $ERROR_MSG"
    exit 1
fi

echo -e "\n${GREEN}=== Test completed successfully ===${NC}"
