cmake_minimum_required (VERSION 3.0)
project (libpixyusb CXX)

add_library (pixyusb STATIC ../../src/chirpreceiver.cpp
                            ../../src/pixyinterpreter.cpp
                            ../../src/pixy.cpp
                            ../../src/usblink.cpp
                            ../../src/utils/timer.cpp
                            ../../../../common/chirp.cpp)

set (THIS_LIB libpixyusb)

include_directories (../../src 
                     ../../src/util
                     ../../include
                     ../../../../common
                     ${Boost_INCLUDE_DIR}
                     ${LIBUSB_INCLUDE})
