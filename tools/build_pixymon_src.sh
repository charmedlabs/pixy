#!/bin/sh

set -x # echo

TARGET_BUILD_FOLDER=../build

rm -fr $TARGET_BUILD_FOLDER

mkdir $TARGET_BUILD_FOLDER
mkdir $TARGET_BUILD_FOLDER/pixymon/
mkdir $TARGET_BUILD_FOLDER/pixymon/src
mkdir $TARGET_BUILD_FOLDER/pixymon/src/host

echo "Creating build folder..."
cp -r ../common             $TARGET_BUILD_FOLDER/pixymon/src
cp -r ../host/pixymon       $TARGET_BUILD_FOLDER/pixymon/src/host
cp -r ../host/libpixyusb       $TARGET_BUILD_FOLDER/pixymon/src/host
#cp -r ../host/libpixy       $TARGET_BUILD_FOLDER/pixymon/src/host
cp ../host/buildpixymon.sh  $TARGET_BUILD_FOLDER/pixymon/

echo "Starting Build..."
cd $TARGET_BUILD_FOLDER/pixymon
chmod +x buildpixymon.sh
./buildpixymon.sh


