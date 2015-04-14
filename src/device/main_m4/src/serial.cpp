//
// begin license header
//
// This file is part of Pixy CMUcam5 or "Pixy" for short
//
// All Pixy source code is provided under the terms of the
// GNU General Public License v2 (http://www.gnu.org/licenses/gpl-2.0.html).
// Those wishing to use Pixy source code, software and/or
// technologies under different licensing terms should contact us at
// cmucam@cs.cmu.edu. Such licensing terms are available for
// all portions of the Pixy codebase presented here.
//
// end license header
//

#include "serial.h"
#include "spi.h"
#include "i2c.h"
#include "uart.h"
#include "analogdig.h"
#include "conncomp.h"
#include "param.h"

static uint8_t g_interface = 0;
static Iserial *g_serial = 0;

#define LEGO
uint16_t lego_getData(uint8_t *buf, uint32_t buflen);


uint32_t callback(uint8_t *data, uint32_t len)
{
#ifdef LEGO
	return lego_getData(data, len);
#else
	return g_blobs->getBlock(data, len);
#endif
}


int ser_init()
{
	i2c_init(callback);
	spi_init(callback);
	uart_init(callback);
	ad_init();

	ser_loadParams();
		
	return 0;	
}

void ser_loadParams()
{
#ifndef LEGO
	prm_add("Data out port", 0, 
		"@c Interface Selects the port that's used to output data, 0=Arduino ICSP SPI, 1=SPI with SS, 2=I2C, 3=UART, 4=analog/digital x, 5=analog/digital y (default 0)", UINT8(0), END);
	prm_add("I2C address", PRM_FLAG_HEX_FORMAT, 
		"@c Interface Sets the I2C address if you are using I2C data out port. (default 0x54)", UINT8(I2C_DEFAULT_SLAVE_ADDR), END);
	prm_add("UART baudrate", 0, 
		"@c Interface Sets the UART baudrate if you are using UART data out port. (default 19200)", UINT32(19200), END);

	uint8_t interface, addr;
	uint32_t baudrate;

	prm_get("Data out port", &interface, END);
	ser_setInterface(interface);

	prm_get("I2C address", &addr, END);
	g_i2c0->setSlaveAddr(addr);

	prm_get("UART baudrate", &baudrate, END);
	g_uart0->setBaudrate(baudrate);
#else
	ser_setInterface(SER_INTERFACE_I2C);
 	g_i2c0->setSlaveAddr(0x01);
#endif
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
	case SER_INTERFACE_SS_SPI:
		g_serial = g_spi;
		g_spi->setAutoSlaveSelect(false);
		break;

	case SER_INTERFACE_I2C:     
		g_serial = g_i2c0;
		break;

	case SER_INTERFACE_UART:    
		g_serial = g_uart0;
		break;

	case SER_INTERFACE_ADX:      
		g_ad->setDirection(true);
		g_serial = g_ad;
		break;

	case SER_INTERFACE_ADY:
		g_ad->setDirection(false);
		g_serial = g_ad;
		break;		

	default:
	case SER_INTERFACE_ARDUINO_SPI:
		g_serial = g_spi;
		g_spi->setAutoSlaveSelect(true);
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
