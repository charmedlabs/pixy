#!/bin/bash

TARGET_BUILD_FOLDER=../build

mkdir $TARGET_BUILD_FOLDER
mkdir $TARGET_BUILD_FOLDER/libpixyusb
cd $TARGET_BUILD_FOLDER/libpixyusb

# Abort script on error
set -e

if [ "$1" == "debug" ]
then
  # Build with debug symbols and logging
  echo "Building DEBUG version."
  cmake ../../src/host/libpixyusb -DCMAKE_CXX_FLAGS="-DDEBUG -g"
  make VERBOSE=1
else
  cmake ../../src/host/libpixyusb $@
  make
fi

set +e

if [ $? -eq 0 ]; then
  echo ""
  echo "-- libpixyusb build complete"
  echo "--"
  echo "-- Please run 'install_libpixyusb.sh' as root to install to your system."
fi
