#!/bin/bash
# Install TinyXML and zlib as local static libraries for self-contained build

set -e  # Exit on any error

echo "Installing TinyXML and zlib as static dependencies..."

# Create directory structure
mkdir -p deps-install/{include,lib}
cd deps-install

# Install TinyXML
echo "Building TinyXML from source..."
TINYXML_VERSION="2.6.2"
TINYXML_URL="https://sourceforge.net/projects/tinyxml/files/tinyxml/${TINYXML_VERSION}/tinyxml_2_6_2.tar.gz"

wget "$TINYXML_URL" -O tinyxml.tar.gz
tar -xzf tinyxml.tar.gz
cd tinyxml

# Build static library with -fPIC for shared library compatibility
echo "Compiling TinyXML with -fPIC..."
g++ -c -fPIC -O2 *.cpp
ar rcs libtinyxml.a *.o

# Install
echo "Installing TinyXML..."
cp *.h ../include/
cp libtinyxml.a ../lib/

cd ..
rm -rf tinyxml tinyxml.tar.gz

# Install zlib
echo "Building zlib from source..."
ZLIB_VERSION="1.3.1"
ZLIB_URL="https://github.com/madler/zlib/releases/download/v${ZLIB_VERSION}/zlib-${ZLIB_VERSION}.tar.gz"

wget "$ZLIB_URL" -O zlib.tar.gz
tar -xzf zlib.tar.gz
cd zlib-${ZLIB_VERSION}

# Configure and build static library with -fPIC
echo "Configuring zlib..."
CFLAGS="-fPIC -O2" ./configure --static --prefix=$PWD/../zlib-install

echo "Building zlib..."
make

echo "Installing zlib..."
make install
cp ../zlib-install/include/*.h ../include/
cp ../zlib-install/lib/libz.a ../lib/

cd ..
rm -rf zlib-${ZLIB_VERSION} zlib.tar.gz zlib-install

echo "Dependencies successfully installed to ./deps-install/"
echo "Headers: ./deps-install/include/"
echo "Libraries: ./deps-install/lib/"

# List what was installed
echo ""
echo "Installed files:"
echo "Headers:"
ls -la include/
echo ""
echo "Libraries:"
ls -la lib/