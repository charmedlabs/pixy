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
#include "spifi_rom_api.h"
#include "pixy_init.h"
#include "flash.h"
#include "flashprog.h"

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
	(ProcPtr)flash_program2, 
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

	mapsize = g_spifi.memSize/FLASH_SECTOR_SIZE;
	 
	g_eraseMap = new uint8_t[mapsize];
	memset((void *)g_eraseMap, 0, mapsize);
}

int32_t erase(uint32_t addr)
{
	// if we've already erased, just return 
	if (g_eraseMap[(addr-FLASH_BEGIN_B)/FLASH_SECTOR_SIZE]==1)
		return 0;

	flash_erase(addr, 1);

	// update map
	g_eraseMap[(addr-FLASH_BEGIN_B)/FLASH_SECTOR_SIZE] = 1;

	return 0;
}


uint32_t flash_sectorSize()
{
	return FLASH_SECTOR_SIZE; 
}

int32_t flash_program2(const uint32_t &addr, const uint32_t &len, const uint8_t *data)
{
	uint32_t i, laddr = addr;

	// add offset
	if (laddr>=FLASH_BEGIN && laddr<=FLASH_END)
		laddr += FLASH_OFFSET;

	// check range
	if (laddr<FLASH_BEGIN_B || laddr>FLASH_END_B || laddr+len>FLASH_END_B)
		return -1;

	// erase all sectors spanned by this segment
	for (i=FLASH_SECTOR_MASK(laddr); i<laddr+len; i+=FLASH_SECTOR_SIZE)
	{
		if (erase(i)<0)
			return -3;
	}
	
	if (flash_program(laddr, data, len)<0)
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


