#include "serial.h"
#include "spi.h"
#include "i2c.h"
#include "analogdig.h"
#include "conncomp.h"

static uint8_t g_interface = 0;
static Iserial *g_serial = 0;

uint32_t callback(uint8_t *data, uint32_t len)
{
	return g_blobs->getBlock(data, len);
}


int ser_init()
{
	i2c_init(callback);
	spi_init(callback);
	ad_init();

	// set default interface
	ser_setInterface(SER_INTERFACE_ADX);
	
	return 0;	
}

int ser_setInterface(uint8_t interface)
{
	if (interface>SER_INTERFACE_ADY)
		return -1;

	if (g_serial!=NULL)
		g_serial->close();

	g_interface = interface;

	switch (interface)
	{
	case SER_INTERFACE_SPI:
		g_serial = g_spi;
		break;
		    
	case SER_INTERFACE_I2C:     
		g_serial = g_i2c0;
		break;

	case SER_INTERFACE_UART:    
		//g_serial = ;
		break;

	case SER_INTERFACE_ADX:      
		g_ad->setDirection(true);
		g_serial = g_ad;
		break;

	case SER_INTERFACE_ADY:
		g_ad->setDirection(false);
		g_serial = g_ad;
		break;		
	}

	g_serial->open();

	return 0;
}

uint8_t ser_getInterface()
{
	return g_interface;
}

Iserial *ser_getSerial()
{
	return g_serial;
}
