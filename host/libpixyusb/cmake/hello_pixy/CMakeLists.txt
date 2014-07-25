cmake_minimum_required (VERSION 2.8)
project (hello_pixy C)

add_executable (hello_pixy ../../example/hello_pixy.c)

# libpixyusb should always come before libboost and libusb #
target_link_libraries (hello_pixy pixyusb)

target_link_libraries (hello_pixy ${Boost_LIBRARIES})
target_link_libraries (hello_pixy ${LIBUSB_1_LIBRARY})

file(STRINGS "../../VERSION" LIBPIXY_VERSION)
add_definitions(-D__LIBPIXY_VERSION__="${LIBPIXY_VERSION}")

include_directories (../../src 
                     ../../include
                     ../../../../common
                     ${Boost_INCLUDE_DIR}
                     ${LIBUSB_1_INCLUDE_DIRS})

