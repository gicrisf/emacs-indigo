#!/bin/bash
# Install Indigo cheminformatics library as local static library for self-contained build

set -e  # Exit on any error

echo "Installing Indigo cheminformatics library..."

# Create directory structure
mkdir -p indigo-install/{include,lib}

# Check if Indigo is already installed
if [ -n "$(ls -A indigo-install/lib/ 2>/dev/null)" ] && [ -n "$(ls -A indigo-install/include/ 2>/dev/null)" ]; then
    echo "Indigo already installed, skipping download."
    echo "To reinstall, remove the indigo-install directory first."
    exit 0
fi

cd indigo-install

# Try downloading Ubuntu .deb package
echo "Trying Ubuntu .deb package extraction..."
mkdir -p downloads
cd downloads

# Latest .deb package URL (as of 2024-04-22)
DEB_URL="http://archive.ubuntu.com/ubuntu/pool/universe/i/indigo/libindigo-dev_1.2.3-3.1build1_amd64.deb"

if wget -q "$DEB_URL" 2>/dev/null || curl -s -O "$DEB_URL" 2>/dev/null; then
    DEB_FILE=$(basename "$DEB_URL")
    echo "Downloaded $DEB_FILE, extracting..."

    # Extract .deb package using dpkg-deb (handles all compression formats)
    echo "Extracting .deb package..."
    dpkg-deb -x "$DEB_FILE" ./extracted/

    # Copy files from extracted directory
    if [ -d "extracted/usr/include" ]; then
        cp extracted/usr/include/indigo*.h ../include/ 2>/dev/null || true
    fi
    if [ -d "extracted/usr/lib" ]; then
        cp extracted/usr/lib/libindigo* ../lib/ 2>/dev/null || true
    fi

    # Verify files were copied
    if [ ! -f "../include/indigo.h" ] || [ ! -f "../lib/libindigo-static.a" ]; then
        echo "Error: Failed to extract Indigo files from .deb package"
        cd ../..
        exit 1
    fi

    echo "Successfully extracted from .deb package"
    cd ..
    rm -rf downloads
else
    cd ..
    rm -rf downloads
    echo "Error: Could not download Indigo .deb package"
    echo "Please manually install Indigo and place headers in ./indigo-install/include/"
    echo "and libraries in ./indigo-install/lib/"
    echo ""
    echo "On Ubuntu/Debian, you can install with:"
    echo "  sudo apt-get install libindigo-dev"
    echo "Then copy files from system locations to this directory."
    exit 1
fi

echo "Indigo successfully installed to ./indigo-install/"
echo "Headers: ./indigo-install/include/"
echo "Libraries: ./indigo-install/lib/"

# List what was installed
echo ""
echo "Installed files:"
echo "Headers:"
ls -la include/ 2>/dev/null || echo "No headers found"
echo ""
echo "Libraries:"
ls -la lib/ 2>/dev/null || echo "No libraries found"