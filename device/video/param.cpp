#include <inttypes.h>
#include <string.h>
#include "param.h"
#include "spifi_rom_api.h"
#include "debug.h"

#define SECTOR_SIZE      0x1000
#define SECTOR_MASK(a)	 (a&(~(SECTOR_SIZE-1)))
#define FLASH_SIZE       g_spifi.memSize
#define FLASH_BEGIN_A	 0x14000000
#define FLASH_BEGIN_B	 g_spifi.base
#define FLASH_END_A		 (FLASH_BEGIN_A+FLASH_SIZE)
#define FLASH_END_B		 (FLASH_BEGIN_B+FLASH_SIZE)
#define FLASH_OFFSET     (FLASH_BEGIN_B-FLASH_BEGIN_A)

#define PRM_MAX_LEN       256
#define PRM_HEADER_LEN    4
#define PRM_DATA_LEN      (PRM_MAX_LEN-PRM_HEADER_LEN)
#define PRM_ALLOCATED_LEN (SECTOR_SIZE*2) // 2 sectors
#define PRM_FLASH_LOC	  (FLASH_BEGIN_A + FLASH_SIZE - PRM_ALLOCATED_LEN)  // last sectors
#define PRM_ENDREC_OFFSET ((PRM_ALLOCATED_LEN/PRM_MAX_LEN)*PRM_MAX_LEN)  // last sector
#define PRM_ENDREC	      (PRM_FLASH_LOC + PRM_ENDREC_OFFSET)  // last sector

static const ProcModule g_module[] =
{
	{
	"prm_set",
	(ProcPtr)prm_setChirp, 
	{CRP_STRING, CRP_INTS8, END}, 
	"Set parameter value"
	"@p parameter identifier (string)"
	"@p parameter value (encoded)"
	"@r 0 if success, negative if error"
	},
	{
	"prm_get",
	(ProcPtr)prm_getChirp, 
	{CRP_STRING, END}, 
	"Get parameter value"
	"@p parameter identifier (string)"
	"@r 0 if success, negative if error"
	},
	{
	"prm_getInfo",
	(ProcPtr)prm_getInfo, 
	{CRP_STRING, END}, 
	"Get parameter information"
	"@p parameter identifier (string)"
	"@r 0 if success, negative if error"
	},
	{
	"prm_getAll",
	(ProcPtr)prm_getAll, 
	{CRP_STRING, END}, 
	"Get parameter information"
	"@p parameter identifier (string)"
	"@r 0 if success, negative if error"
	},
	END
};

int32_t flash_erase(uint32_t addr, uint32_t len)
{
	SPIFIopers spifi;
	uint32_t i;

	// add offset
	if (addr>=FLASH_BEGIN_A && addr<=FLASH_END_A)
		addr += FLASH_OFFSET;

	memset((void *)&spifi, 0, sizeof(spifi));

	for (i=0; i<len; i+=SECTOR_SIZE)
	{ 
		addr = SECTOR_MASK(addr+i);
		spifi.dest = (char *)addr;
		spifi.length = SECTOR_SIZE;
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
	if (addr>=FLASH_BEGIN_A && addr<=FLASH_END_A)
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

struct ParamRecord
{
	uint16_t crc;
	uint16_t len;
	uint8_t data[PRM_DATA_LEN];
};

int prm_init(Chirp *chirp)
{
	chirp->registerModule(g_module);

//	prm_format();
	prm_add("hello", "this is the description",  UINT32(0xdead), UINT16(0xbeef), UINT8(0xab), UINT32(0xbaad), END);
	prm_add("there", "this is another description",  UINT32(0xbaadf00d), UINT16(0xbeef), UINT8(0xab), UINT32(0xbaad), END);
	prm_verifyAll();
	
	uint32_t dead, beef, baad;
	uint8_t ab;

	prm_get("hello", &dead, &beef, &ab, &baad, END);	
	prm_get("there", &dead, &beef, &ab, &baad, END);	
	prm_get("you", &dead, &beef, &ab, &baad, END);

	prm_set("there", UINT32(0xabcdef12), UINT16(0xdeed), UINT8(0xc1), UINT32(0xbaadf00d), END);
	prm_get("there", &dead, &beef, &ab, &baad, END);	
		
	return 0;	
}

const char *prm_getId(ParamRecord *prm)
{
	return (char *)prm+PRM_HEADER_LEN;
}

const char *prm_getDesc(ParamRecord *prm)
{
	uint32_t offset = PRM_HEADER_LEN;
	offset += strlen((char *)prm+offset) + 1;
	return (char *)prm+offset;
}

uint32_t prm_getDataOffset(const ParamRecord *record)
{
	uint32_t offset = PRM_HEADER_LEN;

	offset += strlen((char *)record+offset) + 1;
	offset += strlen((char *)record+offset) + 1;

	// skip padding
	while(*((uint8_t *)record+offset)==0)
		offset++;

	return offset; 
}

int32_t  prm_getInfo(const char *id, Chirp *chirp)
{
	ParamRecord *prm;

	for (prm=(ParamRecord *)PRM_FLASH_LOC; prm->crc!=0xffff && prm<(ParamRecord *)PRM_ENDREC; prm++)
	{
		if(strcmp(id, (char *)prm->data)==0)
		{
			CRP_RETURN(chirp, STRING(prm_getDesc(prm)));
			return 0;
		}
	}
	return -1;	
}


int32_t  prm_getAll(const uint16_t &index, Chirp *chirp)
{
	uint16_t i;
	ParamRecord *prm;

	for (i=0, prm=(ParamRecord *)PRM_FLASH_LOC; prm->crc!=0xffff && prm<(ParamRecord *)PRM_ENDREC; i++, prm++)
	{
		if(i==index)
		{
			CRP_RETURN(chirp, STRING(prm_getId(prm)), STRING(prm_getDesc(prm)),  UINTS8(prm->len, (uint8_t *)prm+prm_getDataOffset(prm)));
			return 0;
		}
	}
	return -1;	
}


int prm_format()
{
	return flash_erase(PRM_FLASH_LOC, PRM_ALLOCATED_LEN);
}

uint16_t prm_crc(const ParamRecord *record)
{
	uint16_t crc;

	crc = Chirp::calcCrc((uint8_t *)record+2, record->len+prm_getDataOffset(record)-2); // +2, -2 because we don't include crc 

	// crc can't equal 0xffff
	if (crc==0xffff)
		crc = 0;

	return crc;
}

ParamRecord *prm_find(const char *id, uint8_t *buf=NULL)
{
	ParamRecord *prm, *begin, *end;

	if (buf)
	{
		begin =  (ParamRecord *)buf;
		end = (ParamRecord *)(buf+SECTOR_SIZE);
	}
	else
	{
		begin =	(ParamRecord *)PRM_FLASH_LOC;
		end = (ParamRecord *)PRM_ENDREC;
	}
	
	for (prm=begin; prm->crc!=0xffff && prm<end; prm++)
	{
		if(strcmp(id, (char *)prm->data)==0)
			return prm;
	}
	return NULL;
}

uint32_t prm_nextFree()
{
	ParamRecord *prm;

	for (prm=(ParamRecord *)PRM_FLASH_LOC; prm->crc!=0xffff && prm<(ParamRecord *)PRM_ENDREC; prm++);

	if (prm>=(ParamRecord *)PRM_ENDREC)
		return NULL;
	return (uint32_t)prm; 
}

bool prm_verifyRecord(const ParamRecord *record)
{	
	return prm_crc(record)==record->crc;
}

bool prm_verifyAll()
{
	ParamRecord *prm;

	for (prm=(ParamRecord *)PRM_FLASH_LOC; prm->crc!=0xffff && prm<(ParamRecord *)PRM_ENDREC; prm++)
	{
		if (prm_verifyRecord(prm)==false)
			return false;
	}

	return true;
}

int32_t prm_set(const char *id, ...)
{
	va_list args;
	int res;
   	uint8_t buf[PRM_MAX_LEN];

	va_start(args, id);
	res = Chirp::vserialize(NULL, buf, PRM_MAX_LEN, &args);
	va_end(args);
	if (res<0)
		return res;

	prm_setChirp(id, res, buf);

	return 0;
}

int32_t prm_setChirp(const char *id, const uint32_t &valLen, const uint8_t *val)
{
	ParamRecord *prm;
	uint8_t *buf;
	uint32_t offset;
	void *sector;

	buf = (uint8_t *)malloc(SECTOR_SIZE);

	if (buf==NULL)
		return -2;

	prm = prm_find(id);

	if (prm==NULL)
		return -1;

	sector = (void *)SECTOR_MASK((uint32_t)prm);
	memcpy(buf, sector, SECTOR_SIZE);

	prm = prm_find(id, buf);

	if (prm==NULL)
		return -1;

	offset = prm_getDataOffset(prm);	
	memcpy((uint8_t *)prm+offset, val, valLen);
	 	
	prm->len = valLen;
	prm->crc = prm_crc(prm);
	
	flash_erase((uint32_t)sector, SECTOR_SIZE); 
	flash_program((uint32_t)sector, buf, SECTOR_SIZE);

	free(buf); 	
	return 0;
}

int32_t prm_get(const char *id, ...)
{
	va_list args;
	ParamRecord *prm;
	int res;

	prm = prm_find(id);
	if (prm==NULL)
		return -1;
	
	va_start(args, id);
	res = Chirp::vdeserialize((uint8_t *)prm+prm_getDataOffset(prm), prm->len, &args);
	va_end(args);
	 	
	return res;
}

int32_t prm_getChirp(const char *id, Chirp *chirp)
{
	ParamRecord *prm;

	prm = prm_find(id);
	if (prm==NULL)
		return -1;
	
	CRP_RETURN(chirp, UINTS8(prm->len, (uint8_t *)prm+prm_getDataOffset(prm)), END);

	return 0;
}


int prm_add(const char *id, const char *desc, ...)
{
	char buf[PRM_MAX_LEN];
	int len;
    uint32_t freeLoc, offset=PRM_HEADER_LEN;
    va_list args;
	ParamRecord *record = (ParamRecord *)buf;

	// if it already exists, 
	if (prm_find(id))
		return -2;

	memset((void *)record, 0, PRM_MAX_LEN);

	strcpy((char *)record+offset, id);
	offset += strlen(id) + 1;
	if (desc!=NULL)
	{
		strcpy((char *)record+offset, desc);
		offset += strlen(desc) + 1;
	}
	else
	{
		*(char *)(record+offset) = '\0';
	 	offset++;
	}

	// data section should be aligned to 4 bytes	
	ALIGN(offset, 4);

    va_start(args, desc);
    len = Chirp::vserialize(NULL, (uint8_t *)record+offset, PRM_MAX_LEN-offset, &args);
    va_end(args);

	if (len<0)
		return -3;

	record->len = len;
	record->crc = prm_crc(record); 

	if ((freeLoc=prm_nextFree())==NULL)
		return -4;
	
	return flash_program(freeLoc, (uint8_t *)record, len+prm_getDataOffset(record));	
}
