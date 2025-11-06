#!/bin/bash
# install.sh - One-command installation for Emacs Indigo

set -e  # Exit on error

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Change to the repository root
cd "$SCRIPT_DIR"

echo "=== Installing Emacs Indigo ==="
echo "Working directory: $(pwd)"
echo ""

echo "Step 1/3: Installing dependencies (zlib, TinyXML)..."
./install-dependencies.sh

echo ""
echo "Step 2/3: Installing Indigo library..."
./install-indigo.sh

echo ""
echo "Step 3/3: Building Emacs module..."
make

echo ""
echo "=== Installation complete! ==="
echo ""
echo "Add to your Emacs config:"
echo "  (add-to-list 'load-path \"$SCRIPT_DIR\")"
echo "  (require 'indigo)"
echo ""
echo "Optional: Run 'cd $SCRIPT_DIR && make test' to verify Indigo installation"
