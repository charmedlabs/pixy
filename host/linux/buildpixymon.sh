#!/bin/bash     
cd src/host/pixymon
qmake pixymon.pro -o Makefile 
make
cd ..


