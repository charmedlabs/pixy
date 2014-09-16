#!/bin/bash

TARGET_BUILD_FOLDER=../build

mkdir $TARGET_BUILD_FOLDER
mkdir $TARGET_BUILD_FOLDER/pixymon/
mkdir $TARGET_BUILD_FOLDER/pixymon/src
mkdir $TARGET_BUILD_FOLDER/pixymon/src/host

echo "Creating build folder..."
cp -r ../src/common             $TARGET_BUILD_FOLDER/pixymon/src
cp -r ../src/host/pixymon       $TARGET_BUILD_FOLDER/pixymon/src/host
cp ../src/host/buildpixymon.sh  $TARGET_BUILD_FOLDER/pixymon/

echo "Starting build..."
cd $TARGET_BUILD_FOLDER/pixymon
chmod +x buildpixymon.sh
./buildpixymon.sh
