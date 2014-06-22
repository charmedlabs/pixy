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

all: build_example

build_example:
	@echo "=========================================================="
	@echo "  Building example"
	@echo "=========================================================="

	@echo -n "OS: "
	@echo $(OS)

	$(COMPILER) -c $(DEFINES) $(INCLUDES) $(TARGETS) 
	$(COMPILER) $(OBJECTS) $(THREAD) $(LIBRARIES) -o ../hello_pixy
	rm *.o
