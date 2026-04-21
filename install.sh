#!/bin/bash
# install.sh - One-command installation for Emacs Indigo

set -e  # Exit on error

# Platform argument (required)
PLATFORM="${1:-}"
if [ -z "$PLATFORM" ]; then
    echo "Usage: $0 <platform>"
    echo "Available platforms: linux-x86_64"
    exit 1
fi

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Change to the repository root
cd "$SCRIPT_DIR"

echo "=== Installing Emacs Indigo ==="
echo "Platform: $PLATFORM"
echo "Working directory: $(pwd)"
echo ""

echo "Step 1/3: Installing dependencies (zlib, TinyXML)..."
bash ./install-dependencies.sh

echo ""
echo "Step 2/3: Installing Indigo library..."
bash ./install-indigo.sh "$PLATFORM"

echo ""
echo "Step 3/3: Building Emacs module..."
make module

echo ""
echo "=== Installation complete! ==="
echo ""
echo "Add to your Emacs config:"
echo "  (add-to-list 'load-path \"$SCRIPT_DIR\")"
echo "  (require 'indigo)"
echo ""
echo "Optional: Run 'cd $SCRIPT_DIR && make test' to verify Indigo installation"
