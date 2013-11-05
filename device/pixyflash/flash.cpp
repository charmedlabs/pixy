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

uint8_t g_resetFlag = 0;

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
	{
	"flash_reset",
	(ProcPtr)flash_reset, 
	{END}, 
	"Reset processor (execute program)."
	"@r always returns 0"
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
}

int32_t erase(uint32_t addr)
{
	SPIFIopers spifi;

	// if we've already erased, just return 
	if (g_eraseMap[(addr-FLASH_BEGIN_B)/SECTOR_SIZE]==1)
		return 0;

	memset((void *)&spifi, 0, sizeof(spifi));

	addr = SECTOR_MASK(addr);
	spifi.dest = (char *)addr;
	spifi.length = SECTOR_SIZE;
	spifi.scratch = NULL;
	spifi.options = S_VERIFY_ERASE;
	if (spifi_erase(&g_spifi, &spifi)) 
		return -1;

	// update map
	g_eraseMap[(addr-FLASH_BEGIN_B)/SECTOR_SIZE] = 1;

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
	if (laddr<FLASH_BEGIN_B || laddr>FLASH_END_B || laddr+len>FLASH_END_B)
		return -1;

	// erase all sectors spanned by this segment
	for (i=SECTOR_MASK(laddr); i<laddr+len; i+=SECTOR_SIZE)
	{
		if (erase(i)<0)
			return -3;
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

int32_t flash_reset()
{
	g_resetFlag = 1;

	return 0;
}


