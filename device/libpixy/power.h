#ifndef _POWER_H
#define _POWER_H

#define VIN_ADCCHAN   	4
#define V5_ADCCHAN	  	3
#define VBUS_ADCCHAN	5

void pwr_init();

uint32_t pwr_getVin();
uint32_t pwr_get5v();
uint32_t pwr_getVbus();
uint32_t pwr_USBpowered();

#endif
