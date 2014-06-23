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
#include "flash.h"

int32_t flash_erase(uint32_t addr, uint32_t len)
{
	SPIFIopers spifi;
	uint32_t addrTemp;
	uint32_t i;

	// add offset
	if (addr>=FLASH_BEGIN && addr<=FLASH_END)
		addr += FLASH_OFFSET;

	memset((void *)&spifi, 0, sizeof(spifi));

	for (i=0; i<len; i+=FLASH_SECTOR_SIZE)
	{ 
		addrTemp = FLASH_SECTOR_MASK(addr+i);
		spifi.dest = (char *)addrTemp;
		spifi.length = FLASH_SECTOR_SIZE;
		spifi.scratch = NULL;
		spifi.options = S_VERIFY_ERASE;
		if (spifi_erase(&g_spifi, &spifi)) 
			return -1;
	}
	return 0;
}

int32_t flash_program(uint32_t addr, const uint8_t *data, uint32_t len)
{
	SPIFIopers spifi;
	// add offset
	if (addr>=FLASH_BEGIN && addr<=FLASH_END)
		addr += FLASH_OFFSET;

 	memset((void *)&spifi, 0, sizeof(spifi));

	spifi.dest = (char *)addr;
	spifi.length = len;
	spifi.scratch = NULL;
	spifi.options = S_VERIFY_PROG;
	if (spifi_program(&g_spifi, (char *)data, &spifi))
		return -1;
	return 0;
}
