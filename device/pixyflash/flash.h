#ifndef _FLASH_H
#define _FLASH_H

#define VIN_ADCCHAN   	4
#define V5_ADCCHAN	  	3
#define VBUS_ADCCHAN	5

void flash_init();

uint32_t flash_sectorSize();
int32_t flash_program(const uint32_t &addr, const uint32_t &len, const uint8_t *data);
int32_t flash_reset();

#endif
