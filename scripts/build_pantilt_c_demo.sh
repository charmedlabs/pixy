#!/bin/bash

TARGET_BUILD_FOLDER=../build
PROJECT_NAME=pantilt_in_c

mkdir $TARGET_BUILD_FOLDER
mkdir $TARGET_BUILD_FOLDER/$PROJECT_NAME

cd $TARGET_BUILD_FOLDER/$PROJECT_NAME
cmake ../../src/host/pantilt_in_c
make
