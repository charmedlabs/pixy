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

#include <string.h>
#include "serial.h"
#include "spi.h"
#include "i2c.h"
#include "uart.h"
#include "analogdig.h"
#include "conncomp.h"
#include "param.h"
#include "pixy_init.h"
#include "exec.h"

uint8_t g_interface = 0;
int8_t g_angle = 0;
static Iserial *g_serial = 0;

uint16_t lego_getData(uint8_t *buf, uint32_t buflen)
{
	uint8_t c;
	uint16_t d;
	uint16_t numBlobs;
	uint32_t temp, width, height;
	Iserial *serial = ser_getSerial();

	if (serial->receive(&c, 1)==0)
		return 0;

#if 1
	if (c==0x00)
	{
		//printf("0\n");
		char *str = "V0.1";
		strcpy((char *)buf, str);
		return 5;
		//return strlen((char *)str);
	}
	if (c==0x08)
	{
		//printf("8\n");
		char *str = "Pixy";
		strcpy((char *)buf, str);
		return 5;
		//return strlen((char *)str);
	}
	else if (c==0x10)
	{
		//printf("10\n");
		char *str = "Pixy";
		strcpy((char *)buf, str);
		return 5;
		//return strlen((char *)str);
	}
	else 
#endif
	if (c==0x50)
	{
		BlobB *max;
#if 0
		buf[0] = 1;
		buf[1] = 2;
		buf[2] = 3;
		buf[3] = 4;
		buf[4] = 5;
		buf[5] = 6;
		buf[6] = 7;
#else
		max = (BlobB *)g_blobs->getMaxBlob();
		if (max==0)
			memset(buf, 0, 7);
		else if (max==(BlobB *)-1)
			memset(buf, -1, 7);
		else
		{
			width = max->m_right - max->m_left;
			height = max->m_bottom - max->m_top;
			*(uint16_t *)buf = max->m_model; // signature
			temp = ((max->m_left + width/2)*819)>>10;
			buf[2] = temp; // x
			buf[3] = max->m_top + height/2; // y
			temp = (width*819)>>10;
			buf[4] = temp; // width
			buf[5] = height; // height
			if (max->m_model>CL_NUM_SIGNATURES)
			{
				temp = ((int32_t)max->m_angle*91)>>7;
				g_angle = temp;
			}
		}
#endif		
		return 6;
	}
	else if (c==0x60)
	{
		buf[0] = g_angle;
		return 1;
	}
	else if (c>=0x51 && c<=0x57)
	{
#if 0
		buf[0] = 1;
		buf[1] = 2;
		buf[2] = 3;
		buf[3] = 4;
		buf[4] = 5;
#else
		BlobA *max;
		max = g_blobs->getMaxBlob(c-0x50, &numBlobs);
		if (max==0)
			memset(buf, 0, 5);
		else if (max==(BlobA *)-1)
			memset(buf, -1, 5);
		else
		{
			width = max->m_right - max->m_left;
			height = max->m_bottom - max->m_top;
			buf[0] = numBlobs; // number of blocks that match signature
			temp = ((max->m_left + width/2)*819)>>10;
			buf[1] = temp; // x
			buf[2] = max->m_top + height/2;	// y
			temp = (width*819)>>10;
			buf[3] = temp; // width
			buf[4] = height; // height
		}
#endif
		return 5;
	}
	else if (c==0x58)
	{
		BlobB *max;
		if (serial->receive((uint8_t *)&d, 2)<2) // receive cc signature to look for
			return 0;
#if 0
		buf[0] = 1;
		buf[1] = 2;
		buf[2] = 3;
		buf[3] = 4;
		buf[4] = 5;
		buf[5] = 6;
#else
		max = (BlobB *)g_blobs->getMaxBlob(d, &numBlobs); 
		if (max==0)
			memset(buf, 0, 6);
		else if (max==(BlobB *)-1)
			memset(buf, -1, 6);
		else
		{
			width = max->m_right - max->m_left;
			height = max->m_bottom - max->m_top;
			buf[0] = numBlobs; // number of cc blocks that match 
			temp = ((max->m_left + width/2)*819)>>10;
			buf[1] = temp; // x
			buf[2] = max->m_top + height/2; // y
			temp = (width*819)>>10;
			buf[3] = temp; // width
			buf[4] = height; // height
			temp = ((int32_t)max->m_angle*91)>>7;
			buf[5] = temp; // angle
		}
#endif
		return 6;
	}
	else  
	{
#if 0
		static uint8_t c = 0;

		buf[0] = c++;
#else
		//printf("%x\n", c);														  

		if (c==0x42) // this works in port view mode on the ev3's LCD
		{
			BlobA *max;
			max = g_blobs->getMaxBlob();
			if (max==0 || max==(BlobA *)-1)
				buf[0] = 0;
			else
			{
				width = max->m_right - max->m_left;
				temp = ((max->m_left + width/2)*819)>>10;
				buf[0] = temp;
			}
		}
		else
			buf[0] = 1;	 // need to return nonzero value for other inquiries or LEGO brick will think we're an analog sensor

#endif
		return 1;
	}
}


uint32_t callback(uint8_t *data, uint32_t len)
{
	if (g_interface==SER_INTERFACE_LEGO)
		return lego_getData(data, len);
	else
		return g_blobs->getBlock(data, len);
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
		"Selects the port that's used to output data (default Arduino ICSP SPI) @c Interface @s 0=Arduino_ICSP_SPI @s 1=SPI_with_SS @s 2=I2C @s 3=UART @s 4=analog/digital_x @s 5=analog/digital_y @s 6=LEGO_I2C", UINT8(0), END);
	prm_add("I2C address", PRM_FLAG_HEX_FORMAT, 
		"@c Interface Sets the I2C address if you are using I2C data out port. (default 0x54)", UINT8(I2C_DEFAULT_SLAVE_ADDR), END);
	prm_add("UART baudrate", 0, 
		"@c Interface Sets the UART baudrate if you are using UART data out port. (default 19200)", UINT32(19200), END);

	uint8_t interface, addr;
	uint32_t baudrate;

	prm_get("I2C address", &addr, END);
	g_i2c0->setSlaveAddr(addr);

	prm_get("UART baudrate", &baudrate, END);
	g_uart0->setBaudrate(baudrate);

	prm_get("Data out port", &interface, END);
	ser_setInterface(interface);

#else
	ser_setInterface(SER_INTERFACE_LEGO);
#endif
}

int ser_setInterface(uint8_t interface)
{
	if (interface>SER_INTERFACE_LEGO)
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
		g_i2c0->setFlags(false, true);
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

	case SER_INTERFACE_LEGO:
		g_serial = g_i2c0;
 		g_i2c0->setSlaveAddr(0x01);
		g_i2c0->setFlags(true, false);
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
