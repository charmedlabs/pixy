TARGETS    = hello_pixy.c

OBJECTS    = hello_pixy.o

INCLUDES   = -I../include
INCLUDES  += -I../../../common

OS         = $(shell cat ../configuration/OS)

DEFINES    = -D$(OS)
DEFINES   += -D__LIBPIXY_VERSION__="\"$(shell cat ../VERSION)\""

LIBRARIES  = -lpixyusb
LIBRARIES += -lusb-1.0
LIBRARIES += -L../
LIBRARIES += -L$(shell cat ../configuration/LIBUSB_PATH)

COMPILER   = g++

THREAD     = -pthread

BOOST      = $(shell cat ../configuration/BOOST_AVAILABLE 2> /dev/null )
CXX11      = $(shell cat ../configuration/CXX11_AVAILABLE 2> /dev/null )

ifeq ($(BOOST), YES)
DEFINES   += -D__USING_BOOST__
LIBRARIES += -lboost_thread
LIBRARIES += -lboost_system
LIBRARIES += -lboost_chrono
endif

ifeq ($(CXX11), YES)
DEFINES   += -D__USING_CXX11__
CPPFLAGS  += -std=c++11
endif

all: check_config build_example

check_config:
	@test -s ../configuration || { echo "Configuration files not found. Please run ./configure"; exit 1; }
	@test -f ../configuration/READY || { echo "Configuration files not found. Please run ./configure"; exit 1; }

build_example:
	@echo "=========================================================="
	@echo "  Building example"
	@echo "=========================================================="

	@echo -n "OS: "
	@echo $(OS)

	$(COMPILER) -c $(DEFINES) $(INCLUDES) $(TARGETS) 
	$(COMPILER) $(OBJECTS) $(THREAD) $(LIBRARIES) -o ../hello_pixy
	rm *.o
