#include <string.h>
#include "flash.h"

int32_t flash_erase(uint32_t addr, uint32_t len)
{
	SPIFIopers spifi;
	uint32_t i;

	// add offset
	if (addr>=FLASH_BEGIN && addr<=FLASH_END)
		addr += FLASH_OFFSET;

	memset((void *)&spifi, 0, sizeof(spifi));

	for (i=0; i<len; i+=FLASH_SECTOR_SIZE)
	{ 
		addr = FLASH_SECTOR_MASK(addr+i);
		spifi.dest = (char *)addr;
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
