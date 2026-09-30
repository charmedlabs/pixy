#!/bin/bash

set -e

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'     
else
   platform='mac'
fi

echo Building for $platform

cd src/host/pixymon

# Find a qmake that can actually resolve the Qt modules this project needs.
# Debian/Ubuntu historically shipped it as qmake-qt4, then qmake-qt5, and now
# Qt6 via qmake6; distro names vary, so probe the candidates instead of guessing.
pick_qmake()
{
   for candidate in "$@"; do
      command -v "$candidate" > /dev/null 2>&1 || continue
      if "$candidate" pixymon.pro > /dev/null 2>&1 && [ -f Makefile ]; then
         echo "$candidate"
         return 0
      fi
      rm -f Makefile
   done
   return 1
}

if [[ "$platform" == 'linux' ]]; then
   QMAKE=$(pick_qmake qmake qmake6 qmake-qt6 qmake-qt5 qmake) || {
      echo "error: no usable qmake found; install Qt development files, e.g."
      echo "       sudo apt install qt6-base-dev"
      exit 1
   }
   echo "Using $($QMAKE -query QT_VERSION) via $QMAKE"
   $QMAKE pixymon.pro
   make -j"$(nproc 2>/dev/null || echo 1)"
   cd ../../..
   mkdir -p bin
   cp src/host/pixymon/PixyMon bin
   strip bin/PixyMon
   cp src/host/pixymon/pixyflash.bin.hdr bin
fi

if [[ "$platform" == 'mac' ]]; then
   qmake pixymon.pro
   make -j"$(nproc 2>/dev/null || echo 1)"
   cd ../../..
   mkdir -p bin
   cp -rf src/host/pixymon/PixyMon.app bin
   strip bin/PixyMon.app/Contents/MacOS/PixyMon
   cp src/host/pixymon/pixyflash.bin.hdr bin/PixyMon.app/Contents/MacOS
fi
