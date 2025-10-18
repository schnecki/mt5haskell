#!/bin/bash
#
# Test script for MT5 REST API Bridge - Get Symbols List
# Tests symbols_get action
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

echo -e "${BLUE}=== MT5 REST API Bridge - Get Symbols Test ===${NC}\n"

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Get optional group filter from command line
GROUP=${1:-""}

if [ -n "$GROUP" ]; then
    echo -e "${YELLOW}Getting symbols matching group: $GROUP${NC}"
    echo -e "${YELLOW}Usage: $0 [group_filter]${NC}\n"
else
    echo -e "${YELLOW}Getting all symbols${NC}"
    echo -e "${YELLOW}Usage: $0 [group_filter]${NC}"
    echo -e "${YELLOW}Example: $0 EUR (get all EUR symbols)${NC}\n"
fi

# Clean up old response
rm -f "$RESPONSE_FILE"

# Create request
echo -e "${BLUE}Requesting symbols list...${NC}"
cat > "$REQUEST_FILE" << EOF
{
  "action": "symbols_get",
  "data": {
    "group": "$GROUP"
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
    COUNT=$(cat "$RESPONSE_FILE" | jq -r '.count // 0')
    
    echo ""
    echo -e "${GREEN}✓ Symbols retrieved successfully!${NC}"
    echo -e "${BLUE}Count:${NC} $COUNT"
    
    if [ $COUNT -gt 0 ]; then
        echo -e "\n${BLUE}Symbols:${NC}"
        cat "$RESPONSE_FILE" | jq -r '.symbols[]' | head -20
        
        if [ $COUNT -gt 20 ]; then
            echo -e "${YELLOW}... and $((COUNT - 20)) more${NC}"
        fi
    fi
else
    ERROR_CODE=$(cat "$RESPONSE_FILE" | jq -r '.error_code // "N/A"')
    ERROR_MSG=$(cat "$RESPONSE_FILE" | jq -r '.error_message // "Unknown error"')
    
    echo ""
    echo -e "${RED}✗ Symbols get failed${NC}"
    echo -e "${RED}Error Code:${NC} $ERROR_CODE"
    echo -e "${RED}Error Message:${NC} $ERROR_MSG"
    exit 1
fi

echo -e "\n${GREEN}=== Test completed successfully ===${NC}"
