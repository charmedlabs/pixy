#!/bin/bash

if [[ $EUID -ne 0 ]]; then
  echo "Please run this script as root to install libpixyusb."
  exit 1
fi

# Verify that libpixyusb has been built #
if [ -d ../build ]; then
  if [ ! -d ../build/libpixyusb ]; then
    if [ ! -e ../build/libpixyusb/libpixyusb.a ]; then
      READY="FALSE"
    fi
    READY="FALSE"
  fi
else
  READY="FALSE"
fi

if [ "$READY" = "FALSE" ]; then
  echo "Error: libpixyusb binaries not found."
  echo "Please run 'build_libpixyusb.sh' first to build libpixyusb binaries."
else
  # Install libpixyusb #
  
  cd ../build/libpixyusb
  make install
fi
