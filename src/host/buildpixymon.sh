#!/bin/bash

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'     
else
   platform='mac'
fi

echo Building for $platform

cd src/host/pixymon

if [[ "$platform" == 'linux' ]]; then
   qmake-qt4 pixymon.pro
   make -w
   cd ../../..
   mkdir bin
   cp src/host/pixymon/PixyMon bin
   strip bin/PixyMon
   cp src/host/pixymon/pixyflash.bin.hdr bin
fi

if [[ "$platform" == 'mac' ]]; then
   qmake pixymon.pro
   make -w
   cd ../../..
   mkdir bin
   cp -rf src/host/pixymon/PixyMon.app bin
   strip bin/PixyMon.app/Contents/MacOS/PixyMon
   cp src/host/pixymon/pixyflash.bin.hdr bin/PixyMon.app/Contents/MacOS
fi
 
