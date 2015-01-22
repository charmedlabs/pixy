#!/bin/bash
rm -rf new
mkdir new
cp -r Pixy new
cd new
zip -r arduino_pixy.zip Pixy
