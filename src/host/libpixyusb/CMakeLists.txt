cmake_minimum_required (VERSION 2.8)
project (libpixyusb CXX)

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake" )
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_PATH} )
set (Boost_USE_STATIC_LIBS OFF)
set (Boost_USE_MULTITHREADED ON)

find_package ( libusb-1.0 REQUIRED )
find_package ( Boost 1.49 COMPONENTS thread system REQUIRED)

# Define Operating System #

IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
add_definitions(-D__MACOS__)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")

IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
add_definitions(-D__LINUX__)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")


file(STRINGS "cmake/VERSION" LIBPIXY_VERSION)
add_definitions(-D__LIBPIXY_VERSION__="${LIBPIXY_VERSION}")


add_library (pixyusb STATIC src/chirpreceiver.cpp
                            src/pixyinterpreter.cpp
                            src/pixy.cpp
                            src/usblink.cpp
                            src/utils/timer.cpp
                            ../../common/src/chirp.cpp)

set (THIS_LIB libpixyusb)

include_directories (src
                     src/util
                     include
                     ../../common/inc
                     ${Boost_INCLUDE_DIR}
                     ${LIBUSB_1_INCLUDE_DIRS})

install (TARGETS pixyusb DESTINATION lib)
install (FILES include/pixy.h DESTINATION include)
install (FILES ../../common/inc/pixydefs.h DESTINATION include)
