#!/bin/bash     
cd src/host/pixymon
qmake pixymon.pro -o Makefile 
make
cd ../../..
cp src/host/pixymon/PixyMon .
strip PixyMon

