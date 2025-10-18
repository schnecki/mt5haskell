#!/bin/bash
#
# Test script for MT5 REST API Bridge - Get Candles (From Position Mode)
# Tests candles_get action with from_pos mode
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

echo -e "${BLUE}=== MT5 REST API Bridge - Get Candles (From Position Mode) Test ===${NC}\n"

# Check directory
if [ ! -d "$MT5_FILES_DIR" ]; then
    echo -e "${RED}ERROR: MT5 Files directory not found${NC}"
    exit 1
fi

# Get parameters from command line or use defaults
SYMBOL=${1:-"EURUSD"}
TIMEFRAME=${2:-"H1"}
POSITION=${3:-0}  # 0 = most recent
COUNT=${4:-100}

echo -e "${YELLOW}Symbol: $SYMBOL${NC}"
echo -e "${YELLOW}Timeframe: $TIMEFRAME${NC}"
echo -e "${YELLOW}Position: $POSITION (0=most recent)${NC}"
echo -e "${YELLOW}Count: $COUNT${NC}"
echo -e "${YELLOW}Usage: $0 [symbol] [timeframe] [position] [count]${NC}\n"

# Clean up old response
rm -f "$RESPONSE_FILE"

# Create request
echo -e "${BLUE}Requesting $COUNT candles from position $POSITION...${NC}"
cat > "$REQUEST_FILE" << EOF
{
  "action": "candles_get",
  "data": {
    "symbol": "$SYMBOL",
    "timeframe": "$TIMEFRAME",
    "mode": "from_pos",
    "position": $POSITION,
    "count": $COUNT
  }
}
EOF

echo -e "${GREEN}✓ Request file created${NC}\n"

# Wait for response
echo -e "${BLUE}Waiting for MT5 response...${NC}"
TIMEOUT=15
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

# Display response (first part only)
echo -e "${BLUE}=== MT5 Response (truncated) ===${NC}"
cat "$RESPONSE_FILE" | jq '{success, count, symbol, timeframe, candles: .candles[0:3]}'

# Parse response
SUCCESS=$(cat "$RESPONSE_FILE" | jq -r '.success // false')

if [ "$SUCCESS" = "true" ]; then
    COUNT_RESP=$(cat "$RESPONSE_FILE" | jq -r '.count // 0')
    SYMBOL_RESP=$(cat "$RESPONSE_FILE" | jq -r '.symbol // "N/A"')
    TF_RESP=$(cat "$RESPONSE_FILE" | jq -r '.timeframe // "N/A"')
    
    echo ""
    echo -e "${GREEN}✓ Candles retrieved successfully!${NC}"
    echo -e "${BLUE}Symbol:${NC} $SYMBOL_RESP"
    echo -e "${BLUE}Timeframe:${NC} $TF_RESP"
    echo -e "${BLUE}Count:${NC} $COUNT_RESP"
    
    if [ $COUNT_RESP -gt 0 ]; then
        echo -e "\n${BLUE}First candle (most recent):${NC}"
        cat "$RESPONSE_FILE" | jq '.candles[0]'
        
        if [ $COUNT_RESP -gt 1 ]; then
            echo -e "\n${BLUE}Last candle (oldest):${NC}"
            cat "$RESPONSE_FILE" | jq ".candles[$((COUNT_RESP-1))]"
        fi
    fi
else
    ERROR_CODE=$(cat "$RESPONSE_FILE" | jq -r '.error_code // "N/A"')
    ERROR_MSG=$(cat "$RESPONSE_FILE" | jq -r '.error_message // "Unknown error"')
    
    echo ""
    echo -e "${RED}✗ Candles get failed${NC}"
    echo -e "${RED}Error Code:${NC} $ERROR_CODE"
    echo -e "${RED}Error Message:${NC} $ERROR_MSG"
    exit 1
fi

echo -e "\n${GREEN}=== Test completed successfully ===${NC}"
