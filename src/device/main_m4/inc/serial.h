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

#ifndef _SERIAL_H
#define _SERIAL_H
#include "iserial.h"

// different interfaces
#define SER_INTERFACE_ARDUINO_SPI     0	  // arduino ICMP SPI (auto slave select)
#define SER_INTERFACE_SS_SPI          1	  // spi with slave select
#define SER_INTERFACE_I2C             2
#define SER_INTERFACE_UART            3
#define SER_INTERFACE_ADX             4
#define SER_INTERFACE_ADY             5 
#define	SER_INTERFACE_LEGO            6


int ser_init();
int ser_setInterface(uint8_t interface);
uint8_t ser_getInterface();
Iserial *ser_getSerial();

void ser_loadParams();

extern uint8_t g_interface;

#endif
