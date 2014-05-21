#!/bin/bash     
cd src/host/pixymon
qmake pixymon.pro -o Makefile 
make
cd ../../..
mkdir bin
cp src/host/pixymon/PixyMon bin
strip bin/PixyMon
cp src/host/pixymon/pixyflash.bin.hdr bin

