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

#ifndef _FLASH_H
#define _FLASH_H
#include "spifi_rom_api.h"

extern SPIFIobj g_spifi;

#define FLASH_SECTOR_SIZE        	0x1000
#define FLASH_SECTOR_MASK(a)	 	(a&(~(FLASH_SECTOR_SIZE-1)))
#define FLASH_SIZE               	g_spifi.memSize
#define FLASH_BEGIN	             	0x14000000
#define FLASH_BEGIN_B	 			g_spifi.base
#define FLASH_END		 			(FLASH_BEGIN+FLASH_SIZE)
#define FLASH_END_B		 			(FLASH_BEGIN_B+FLASH_SIZE)
#define FLASH_OFFSET     			(FLASH_BEGIN_B-FLASH_BEGIN)

extern void __aeabi_memcpy4(void *dest, const void *src, unsigned int n);
int32_t flash_erase(uint32_t addr, uint32_t len);
int32_t flash_program(uint32_t addr, const uint8_t *data, uint32_t len);

#endif

