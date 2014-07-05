TARGETS    = src/pixy.c \
             src/pixyinterpreter.cpp \
             src/chirpreceiver.cpp \
             src/usblink.cpp \
             src/utils/timer.cpp \
				  	 src/utils/mutex.cpp \
             ../../common/chirp.cpp

OBJECTS    = pixy.o \
             pixyinterpreter.o \
             chirpreceiver.o \
				   	 usblink.o \
			 			 mutex.o \
				  	 timer.o \
				  	 chirp.o

INCLUDES   = -I../../common
INCLUDES  += -Iinclude
INCLUDES  += -I./src
INCLUDES  += -I/usr/include/libusb-1.0/

OS         = $(shell cat configuration/OS)
           
DEFINES    = -D$(OS)
           
COMPILER   = g++
           
CPPFLAGS   = -c
           
THREAD     = -pthread
           
VERSION    = `cat VERSION`

BOOST      = $(shell cat configuration/BOOST_AVAILABLE 2> /dev/null )
CXX11      = $(shell cat configuration/CXX11_AVAILABLE 2> /dev/null )

ifeq ($(BOOST), YES)
DEFINES   += -D__USING_BOOST__
LIBRARIES += -lboost_thread
endif

ifeq ($(CXX11), YES)
DEFINES   += -D__USING_CXX11__
CPPFLAGS  += -std=c++11
endif

all: check_config build_libpixyusb build_example

check_config:
	@test -s configuration || { echo "Configuration files not found. Please run ./configure"; exit 1; }
	@test -f configuration/READY || { echo "Configuration files not found. Please run ./configure"; exit 1; }

build_libpixyusb:
	@echo " "
	@echo "=========================================================="
	@echo "  Building libpixyusb  v$(VERSION)"
	@echo "=========================================================="
	$(COMPILER) $(CPPFLAGS) $(THREAD) $(DEFINES) $(TARGETS) $(LIBRARIES) $(INCLUDES)
	ar rcs libpixyusb.a $(OBJECTS)
	rm *.o

build_example:
	cd example;\
	make

