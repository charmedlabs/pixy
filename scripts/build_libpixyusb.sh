#!/bin/bash

TARGET_BUILD_FOLDER=../build

mkdir $TARGET_BUILD_FOLDER
mkdir $TARGET_BUILD_FOLDER/libpixyusb

cd $TARGET_BUILD_FOLDER/libpixyusb
cmake ../../src/host/libpixyusb
make

if [ $? -eq 0 ]; then
  echo ""
  echo "-- libpixyusb build complete"
  echo "--"
  echo "-- Please run 'install_libpixyusb.sh' as root to install to your system."
fi
