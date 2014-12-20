#!/bin/bash

TARGET_BUILD_FOLDER=../build

mkdir $TARGET_BUILD_FOLDER
mkdir $TARGET_BUILD_FOLDER/hello_pixy

cd $TARGET_BUILD_FOLDER/hello_pixy
cmake ../../src/host/hello_pixy
make
