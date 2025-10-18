#!/bin/bash
#
# MT5 REST API Installation Script
# Copies MQL5 files to MetaTrader 5 directory and sets up Python server
#

set -e  # Exit on error

echo "========================================"
echo "MT5 REST API Installation Script"
echo "========================================"
echo ""

# Colors for output
RED='\033[0|31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
MT5_BASE_PATH="$HOME/.wine/drive_c/Program Files/MetaTrader 5"
MT5_MQL5_PATH="$MT5_BASE_PATH/MQL5"
MT5_EXPERTS_PATH="$MT5_MQL5_PATH/Experts"
MT5_INCLUDE_PATH="$MT5_MQL5_PATH/Include"

PROJECT_MQL5_PATH="$(dirname "$0")/../MQL5"
SCRIPTS_PATH="$(dirname "$0")"

# Functions
print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}!${NC} $1"
}

print_info() {
    echo -e "  $1"
}

check_prerequisites() {
    echo "Checking prerequisites..."

    # Check if Wine MT5 directory exists
    if [ ! -d "$MT5_BASE_PATH" ]; then
        print_error "MetaTrader 5 not found at: $MT5_BASE_PATH"
        echo ""
        echo "Please verify your MT5 installation path and update this script if needed."
        echo "Common paths:"
        echo "  - $HOME/.wine/drive_c/Program Files/MetaTrader 5"
        echo "  - $HOME/.wine/drive_c/Program Files (x86)/MetaTrader 5"
        echo ""
        read -p "Enter custom MT5 path (or press Enter to exit): " CUSTOM_PATH
        if [ -z "$CUSTOM_PATH" ]; then
            exit 1
        fi
        MT5_BASE_PATH="$CUSTOM_PATH"
        MT5_MQL5_PATH="$MT5_BASE_PATH/MQL5"
        MT5_EXPERTS_PATH="$MT5_MQL5_PATH/Experts"
        MT5_INCLUDE_PATH="$MT5_MQL5_PATH/Include"
    fi
    print_success "MetaTrader 5 found at: $MT5_BASE_PATH"

    # Check if project MQL5 directory exists
    if [ ! -d "$PROJECT_MQL5_PATH" ]; then
        print_error "Project MQL5 directory not found at: $PROJECT_MQL5_PATH"
        exit 1
    fi
    print_success "Project MQL5 directory found"

    # Check if Python3 is available
    if ! command -v python3 &> /dev/null; then
        print_error "Python3 not found. Please install Python3."
        exit 1
    fi
    print_success "Python3 found: $(python3 --version)"
}

install_python_dependencies() {
    echo ""
    echo "Installing Python dependencies..."

    if ! python3 -c "import flask" &> /dev/null; then
        print_info "Installing Flask..."
        pipx install flask
        print_success "Flask installed"
    else
        print_success "Flask already installed"
    fi
}

copy_mql5_files() {
    echo ""
    echo "Copying MQL5 files to MetaTrader 5..."

    # Ensure directories exist
    mkdir -p "$MT5_EXPERTS_PATH"
    mkdir -p "$MT5_INCLUDE_PATH"

    # Copy Expert Advisor
    if [ -f "$PROJECT_MQL5_PATH/Experts/MT5RestAPIBridge.mq5" ]; then
        cp "$PROJECT_MQL5_PATH/Experts/MT5RestAPIBridge.mq5" "$MT5_EXPERTS_PATH/"
        print_success "Copied MT5RestAPIBridge.mq5 to Experts"
    else
        print_error "MT5RestAPIBridge.mq5 not found in project"
        exit 1
    fi

    # Check if JAson.mqh exists
    if [ -f "$MT5_INCLUDE_PATH/JAson.mqh" ]; then
        print_success "JAson.mqh already exists in Include directory"
    else
        cp "$PROJECT_MQL5_PATH/Include/JAson.mqh" "$MT5_INCLUDE_PATH/"
        print_success "Copied JAson.mqh to Include"
        # print_warning "JAson.mqh not found in Include directory"
        # print_info "Please ensure JAson.mqh is available in $MT5_INCLUDE_PATH"
        # print_info "Download from: https://www.mql5.com/en/code/13663"
    fi
}

make_server_executable() {
    echo ""
    echo "Making Python server executable..."

    chmod +x "$SCRIPTS_PATH/mt5_rest_api_server.py"
    print_success "Server script is now executable"
}

print_next_steps() {
    echo ""
    echo "========================================"
    echo "Installation Complete!"
    echo "========================================"
    echo ""
    echo "Next steps:"
    echo ""
    echo "1. Start MetaTrader 5 (in Wine)"
    echo "   wine '$MT5_BASE_PATH/terminal64.exe'"
    echo ""
    echo "2. In MT5, compile the Expert Advisor:"
    echo "   - Press F4 to open MetaEditor"
    echo "   - Open MT5RestAPIBridge.mq5 from Experts folder"
    echo "   - Press F7 to compile"
    echo "   - Check for compilation errors"
    echo ""
    echo "3. Attach EA to a chart:"
    echo "   - Open any chart (e.g., EURUSD)"
    echo "   - Drag MT5RestAPIBridge from Navigator → Expert Advisors"
    echo "   - Enable AutoTrading (Ctrl+E)"
    echo "   - Verify EA is running (smiley face ☺ in chart corner)"
    echo ""
    echo "4. Start the REST API server:"
    echo "   cd scripts"
    echo "   ./mt5_rest_api_server.py"
    echo ""
    echo "5. Test the API:"
    echo "   curl http://localhost:8085/api/health"
    echo ""
    echo "Files Location:"
    echo "  Expert Advisor: $MT5_EXPERTS_PATH/MT5RestAPIBridge.mq5"
    echo "  Python Server:  $SCRIPTS_PATH/mt5_rest_api_server.py"
    echo ""
}

# Main installation flow
main() {
    check_prerequisites
    install_python_dependencies
    copy_mql5_files
    make_server_executable
    print_next_steps
}

# Run installation
main
