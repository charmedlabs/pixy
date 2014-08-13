cmake_minimum_required (VERSION 2.8)

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake" )
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR} )
set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR} )
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR} )
set (Boost_USE_STATIC_LIBS OFF)
set (Boost_USE_MULTITHREADED ON)

find_package ( libusb-1.0 )
find_package ( Boost 1.49 COMPONENTS thread system chrono REQUIRED)

# Define Operating System #

IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
add_definitions(-D__MACOS__)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")

IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
add_definitions(-D__LINUX__)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")

ADD_SUBDIRECTORY( cmake/libpixyusb )
ADD_SUBDIRECTORY( cmake/hello_pixy )

