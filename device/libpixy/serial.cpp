#include "serial.h"

static uint8_t g_interface = 0;
static Iserial *g_serial = 0;

int ser_init()
{
	
}

int ser_setInterface(uint8_t interface)
{
}

uint8_t ser_getInterface()
{
	return g_interface;
}

Iserial *ser_getSerial()
{
	return g_serial;
}
