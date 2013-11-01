#include <string.h>
#include "spifi_rom_api.h"
#include "pixy_init.h"
#include "flash.h"

#define SECTOR_SIZE     0x1000
#define SECTOR_MASK(a)	(a&(~(SECTOR_SIZE-1)))
#define FLASH_SIZE      g_spifi.memSize
#define FLASH_BEGIN_A	0x14000000
#define FLASH_BEGIN_B	g_spifi.base
#define FLASH_END_A		(FLASH_BEGIN_A+FLASH_SIZE)
#define FLASH_END_B		(FLASH_BEGIN_B+FLASH_SIZE)
#define FLASH_OFFSET    (FLASH_BEGIN_B-FLASH_BEGIN_A)


uint8_t *g_eraseMap;

static const ProcModule g_module[] =
{
	{
	"flash_sectorSize",
	(ProcPtr)flash_sectorSize, 
	{END}, 
	"Get size of flash sector"
	"@r sector size in bytes"
	},
	{
	"flash_program",
	(ProcPtr)flash_program, 
	{CRP_INT32, CRP_INTS8, END}, 
	"Program flash (including erasing and verifying)."
	"@p addr destination address"
	"@p data programming data"
	"@p len length of data"
	"@r voltage in millivolts"
	},
	END
};	


void flash_init()
{
	uint32_t mapsize;

   	g_chirpUsb->registerModule(g_module);

	mapsize = g_spifi.memSize/SECTOR_SIZE;
	 
	g_eraseMap = new uint8_t[mapsize];
	memset((void *)g_eraseMap, 0, mapsize);

#if 0	 
	SPIFIopers spifi;
	memset((void *)&spifi, 0, sizeof(spifi));
	char datab[4] = {0, 0x12, 0x34, 0x56};
	spifi.dest = (char *)g_spifi.base;
	spifi.length = g_spifi.memSize;
	spifi.scratch = NULL;
	spifi.options = S_VERIFY_ERASE;
	if (spifi_erase(&g_spifi, &spifi)) 
		return;

	int i;

	for (i=0; i<0x10000; i+=4)
	{
		spifi.dest = (char *)g_spifi.base+i;
		spifi.length = 4;
		spifi.scratch = (char *) NULL;
		spifi.protect = 0;
		spifi.options = S_VERIFY_PROG;
		if (spifi_program(&g_spifi, datab, &spifi))
			break;
	}


	spifi.dest = (char *)g_spifi.base;
	spifi.length = 0x1000;
	spifi.scratch = NULL;
	spifi.options = S_VERIFY_ERASE;

	if (spifi_erase(&g_spifi, &spifi)) 
		return;


	spifi.dest = (char *)g_spifi.base+0x1000;
	spifi.length = 0x1000;
	spifi.scratch = NULL;
	spifi.options = S_VERIFY_ERASE;

	if (spifi_erase(&g_spifi, &spifi)) 
		return;
#endif
}

int32_t erase(uint32_t addr)
{
	SPIFIopers spifi;
	memset((void *)&spifi, 0, sizeof(spifi));

	addr = SECTOR_MASK(addr);
	spifi.dest = (char *)addr;
	spifi.length = SECTOR_SIZE;
	spifi.scratch = NULL;
	spifi.options = S_VERIFY_ERASE;
	if (spifi_erase(&g_spifi, &spifi)) 
		return -1;

	return 0;
}

int32_t program(uint32_t addr, const uint8_t *data, uint32_t len)
{
	SPIFIopers spifi;
 	memset((void *)&spifi, 0, sizeof(spifi));

	spifi.dest = (char *)addr;
	spifi.length = len;
	spifi.scratch = NULL;
	spifi.options = S_VERIFY_PROG;
	if (spifi_program(&g_spifi, (char *)data, &spifi))
		return -1;

	return 0;
}


uint32_t flash_sectorSize()
{
	return SECTOR_SIZE; 
}

int32_t flash_program(const uint32_t &addr, const uint32_t &len, const uint8_t *data)
{
	uint32_t i, laddr = addr;

	// add offset
	if (laddr>=FLASH_BEGIN_A && laddr<=FLASH_END_A)
		laddr += FLASH_OFFSET;

	// check range
	if (laddr<FLASH_BEGIN_B || laddr>FLASH_END_B)
		return -1;

	if (g_eraseMap[(laddr-FLASH_BEGIN_B)/SECTOR_SIZE]==0)
	{
		if (erase(laddr)<0)
			return -3;
		g_eraseMap[(laddr-FLASH_BEGIN_B)/SECTOR_SIZE] = 1;
	}
	
	if (program(laddr, data, len)<0)
		return -2;

	// verify
	for (i=0; i<len; i++)
	{
		if (*(uint8_t *)(laddr+i) != data[i])
			return -4;
	}

	return 0;
}


