cmake_minimum_required (VERSION 2.8)
project (pan_tilt CXX)

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake" )

# Add sources here... #
add_executable (pan_tilt pan_tilt.cpp)

# libpixyusb should always come before libboost and libusb #
target_link_libraries (pan_tilt pixyusb)

find_package ( libpixyusb REQUIRED )
find_package ( libusb-1.0 REQUIRED )
find_package ( Boost 1.49 COMPONENTS thread system chrono REQUIRED)

target_link_libraries (pan_tilt ${Boost_LIBRARIES})
target_link_libraries (pan_tilt ${LIBUSB_1_LIBRARY})

file(STRINGS "cmake/VERSION" LIBPIXY_VERSION)
add_definitions(-D__LIBPIXY_VERSION__="${LIBPIXY_VERSION}")

include_directories (src
                     include
                     ../../common
                     ${Boost_INCLUDE_DIR}
                     ${LIBUSB_1_INCLUDE_DIRS})

