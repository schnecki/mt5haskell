#!/bin/bash

# Copyright 2022, MetaQuotes Ltd.

echo "Install wine(-staging) "
echo "E.g. on ArchLinux: sudo pacman -S wine-staging"

# MetaTrader download url
URL="https://download.mql5.com/cdn/web/metaquotes.software.corp/mt5/mt5setup.exe"
# WebView2 Runtime download url
URL_WEBVIEW="https://msedge.sf.dl.delivery.mp.microsoft.com/filestreamingservice/files/c1336fd6-a2eb-4669-9b03-949fc70ace0e/MicrosoftEdgeWebview2Setup.exe"
# Wine version to install: stable or devel
WINE_VERSION="stable"
# Wine prefix
WINEPRFX="$HOME/.wine"

# Download Python for Wine
wget https://www.python.org/ftp/python/3.9.13/python-3.9.13-amd64.exe && mv python-3.9.13-amd64.exe /tmp

# Download MetaTrader
wget $URL && mv mt5setup.exe /tmp/
# Download WebView2 Runtime
wget $URL_WEBVIEW && mv MicrosoftEdgeWebview2Setup.exe /tmp/

mkdir -p $WINEPRFX
# Set environment to Windows 10
WINEPREFIX=$WINEPRFX winecfg -v=win11
# Install WebView2 Runtime
WINEPREFIX=$WINEPRFX wine /tmp/python-3.9.13-amd64.exe # python-3.13.5-amd64.exe
# Install WebView2 Runtime
WINEPREFIX=$WINEPRFX wine /tmp/MicrosoftEdgeWebview2Setup.exe /silent /install
# Start MetaTrader installer
WINEPREFIX=$WINEPRFX wine /tmp/mt5setup.exe
