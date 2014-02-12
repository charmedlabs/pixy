#ifndef _SERIAL_H
#define _SERIAL_H
#include "iserial.h"

// different interfaces
#define SER_INTERFACE_SPI     0
#define SER_INTERFACE_I2C     1
#define SER_INTERFACE_UART    2
#define SER_INTERFACE_ADX     3
#define SER_INTERFACE_ADY     4


int ser_init();
int ser_setInterface(uint8_t interface);
uint8_t ser_getInterface();
Iserial *ser_getSerial();

#endif
