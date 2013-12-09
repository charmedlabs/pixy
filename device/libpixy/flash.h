#ifndef _FLASH_H
#define _FLASH_H
#include "spifi_rom_api.h"

#define FLASH_SECTOR_SIZE        	0x1000
#define FLASH_SECTOR_MASK(a)	 	(a&(~(FLASH_SECTOR_SIZE-1)))
#define FLASH_SIZE               	g_spifi.memSize
#define FLASH_BEGIN	             	0x14000000
#define FLASH_BEGIN_B	 			g_spifi.base
#define FLASH_END		 			(FLASH_BEGIN+FLASH_SIZE)
#define FLASH_END_B		 			(FLASH_BEGIN_B+FLASH_SIZE)
#define FLASH_OFFSET     			(FLASH_BEGIN_B-FLASH_BEGIN)

int32_t flash_erase(uint32_t addr, uint32_t len);
int32_t flash_program(uint32_t addr, const uint8_t *data, uint32_t len);

#endif

