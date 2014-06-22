#!/bin/bash

PIXY_VERSION=$(cat ../version/PIXYMON)
PIXY_SRC_FOLDER=pixymon-$PIXY_VERSION

echo "Building Pixymon $PIXY_VERSION source packages..."

mkdir $PIXY_SRC_FOLDER
mkdir $PIXY_SRC_FOLDER/src
mkdir $PIXY_SRC_FOLDER/src/host

echo "Copying common and Pixymon sources..."
cp -r ../common             $PIXY_SRC_FOLDER/src
cp -r ../host/pixymon       $PIXY_SRC_FOLDER/src/host

cp ../host/buildpixymon.sh  $PIXY_SRC_FOLDER

echo "Copying OS specific..."
cp ../host/linux/*          $PIXY_SRC_FOLDER
cp ../host/windows/README.* $PIXY_SRC_FOLDER
cp ../host/mac/README.*     $PIXY_SRC_FOLDER

echo "ZIP compression..."
zip -r pixymon_src-$PIXY_VERSION\.zip $PIXY_SRC_FOLDER
echo "TAR/GZ compression..."
tar -zcvf pixymon_src-$PIXY_VERSION\.tar.gz $PIXY_SRC_FOLDER

rm -rf $PIXY_SRC_FOLDER 
