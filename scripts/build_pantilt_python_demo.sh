#!/bin/bash

cd ..

PIXY_ROOT=$PWD
TARGET_BUILD_FOLDER=pantilt_in_python

mkdir $PIXY_ROOT/build/
mkdir $PIXY_ROOT/build/$TARGET_BUILD_FOLDER

cd $PIXY_ROOT/src/host/pantilt_in_python

# Abort script on error
set -e

echo "-----------------------------------------"
echo " Building libpixyusb SWIG module"
echo "-----------------------------------------"

# Pass OS identity to C preprocessor #
OS=`uname`
if [ "$OS" == "Darwin" ]
then
  OS="__MACOS__"
elif [ "$OS" == "Linux" ]
then
  OS="__LINUX__"
fi

cp pan_tilt.py $PIXY_ROOT/build/$TARGET_BUILD_FOLDER
cp pixy.i $PIXY_ROOT/build/$TARGET_BUILD_FOLDER
cp setup.py $PIXY_ROOT/build/$TARGET_BUILD_FOLDER

cd $PIXY_ROOT/build/$TARGET_BUILD_FOLDER

swig -c++ -python pixy.i 

if [ "$1" == "debug" ]
then
  echo "OS is $OS"
  python setup.py build_ext --inplace -D"DEBUG,$OS"
else
  python setup.py build_ext --inplace -D$OS
fi

set +e

echo ""
echo "-----------------------------------------"
echo " Build complete"
echo "-----------------------------------------"
echo ""
echo "To run the get_blocks libpixyusb SWIG example execute the following commands:"
echo ""
echo " > cd ../build/$TARGET_BUILD_FOLDER"
echo " > python pan_tilt.py"
echo ""

cd $PIXY_ROOT/scripts
