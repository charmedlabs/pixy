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

#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include "param.h"
#include "pixytypes.h"
#include "flash.h"
#include "debug.h"
#include "pixy_init.h"
#include "simplevector.h"

#define PRM_MAX_LEN       			256
#define PRM_HEADER_LEN    			8
#define PRM_DATA_LEN      			(PRM_MAX_LEN-PRM_HEADER_LEN)
#define PRM_ALLOCATED_LEN 			(FLASH_SECTOR_SIZE*8) // 8 sectors
#define PRM_FLASH_LOC	  			(FLASH_BEGIN + FLASH_SIZE - PRM_ALLOCATED_LEN)  // last sectors
#define PRM_ENDREC_OFFSET 			((PRM_ALLOCATED_LEN/PRM_MAX_LEN)*PRM_MAX_LEN)  // last sector
#define PRM_ENDREC	      			(PRM_FLASH_LOC + PRM_ENDREC_OFFSET)  // last sector

static const ProcModule g_module[] =
{
	{
	"prm_restore",
	(ProcPtr)prm_format, 
	{END}, 
	"Erase all parameters and restore to default values"
	"@r 0 if success, negative if error"
	},
	{
	"prm_set",
	(ProcPtr)prm_setChirp, 
	{CRP_STRING, CRP_INTS8, END}, 
	"Set parameter value"
	"@p identifier name of parameter (string)"
	"@p value value of parameter (encoded)"
	"@r 0 if success, negative if error"
	},
	{
	"prm_reload",
	(ProcPtr)prm_setDirty, 
	{END}, 
	"Causes all parameters to be reloaded"
	"@r 0 if success, negative if error"
	},
	{
	"prm_setShadow",
	(ProcPtr)prm_setShadowChirp, 
	{CRP_STRING, CRP_INTS8, END}, 
	"Set parameter's shadow value"
	"@p identifier name of parameter (string)"
	"@p value value of parameter (encoded)"
	"@r 0 if success, negative if error"
	},
	{
	"prm_resetShadows",
	(ProcPtr)prm_resetShadows, 
	{END}, 
	"Reset the shadow values of all parameters"
	"@r 0 if success, negative if error"
	},
	{
	"prm_get",
	(ProcPtr)prm_getChirp, 
	{CRP_STRING, END}, 
	"Get parameter value"
	"@p identifier name of parameter (string)"
	"@r 0 if success, negative if error"
	},
	{
	"prm_getInfo",
	(ProcPtr)prm_getInfo, 
	{CRP_STRING, END}, 
	"Get parameter information"
	"@p identifier name of parameter (string)"
	"@r 0 if success, negative if error"
	},
	{
	"prm_getAll",
	(ProcPtr)prm_getAll, 
	{CRP_INT16, END}, 
	"Get all information"
	"@p index index of parameter"
	"@r 0 if success, negative if error"
	},
	END
};

struct ParamRecord
{
	uint16_t crc;
	uint16_t len;
	uint32_t flags;
	uint8_t data[PRM_DATA_LEN];
};

struct Shadow
{
	const char *id; // const data
	uint16_t len;	
	uint8_t *data;
	ShadowCallback callback;
};

static bool g_dirty = false;

static SimpleVector<Shadow> g_shadowTable;

int prm_init(Chirp *chirp)
{
#if 0
	int i, count;
	// check integrity
	if (!prm_verifyAll()) 
	{
		// take a more stochastic approach becuause there may be power-related issues that would cause us to 
		// mis-read.  But if we truly are corrupt, count will not increment. 
		for (i=0, count=0; i<10; i++)
		{
			if (prm_verifyAll())
				count++;
		}	
		// if we're corrupt, format, start over
		if (count==0)
			prm_format();
	} 
#endif

	chirp->registerModule(g_module);
		
	return 0;	
}

Shadow *prm_findShadow(const char *id)
{
	int i;

	for (i=0; i<g_shadowTable.size(); i++)
	{
		if (strcmp(g_shadowTable[i].id, id)==0)
			return &g_shadowTable[i];
	}
	return NULL;
}


int32_t prm_resetShadows()
{
	int i, n;

	for (i=0, n=0; i<g_shadowTable.size(); i++)
	{
		if (g_shadowTable[i].data)
		{
			free(g_shadowTable[i].data);
			g_shadowTable[i].data = NULL;
			g_shadowTable[i].len = 0;
			n++;
		}
	}
	if (n>0)
		g_dirty = true;	// force a reloading of parameters
	return n;
}


const char *prm_getId(ParamRecord *rec)
{
	return (char *)rec+PRM_HEADER_LEN;
}

const char *prm_getDesc(ParamRecord *rec)
{
	uint32_t offset = PRM_HEADER_LEN;
	offset += strlen((char *)rec+offset) + 1;
	return (char *)rec+offset;
}

uint32_t prm_getDataOffset(const ParamRecord *rec)
{
	uint32_t offset = PRM_HEADER_LEN;

	offset += strlen((char *)rec+offset) + 1;
	offset += strlen((char *)rec+offset) + 1;

	// skip padding
	while(*((uint8_t *)rec+offset)==0)
		offset++;

	return offset; 
}

int32_t prm_getInfo(const char *id, Chirp *chirp)
{
	ParamRecord *rec;

	for (rec=(ParamRecord *)PRM_FLASH_LOC; rec->crc!=0xffff && rec<(ParamRecord *)PRM_ENDREC; rec++)
	{
		if(strcmp(id, (char *)rec->data)==0)
		{
			CRP_RETURN(chirp, STRING(prm_getDesc(rec)));
			return 0;
		}
	}
	return -1;	
}


int32_t  prm_getAll(const uint16_t &index, Chirp *chirp)
{
	int res;
	uint16_t i;
	uint32_t len;
	uint8_t *data, argList[CRP_MAX_ARGS];
	ParamRecord *rec;
	Shadow *shadow;

	for (i=0, rec=(ParamRecord *)PRM_FLASH_LOC; rec->crc!=0xffff && rec<(ParamRecord *)PRM_ENDREC; i++, rec++)
	{
		if(i==index)
		{
			shadow = prm_findShadow(prm_getId(rec));

			if (shadow && shadow->data)
			{
				len = shadow->len;
				data = shadow->data;
			}
			else
			{
				len = rec->len;
				data = (uint8_t *)rec+prm_getDataOffset(rec);
			}
			res = Chirp::getArgList(data, rec->len, argList);
			if (res<0)
				return res;
			CRP_RETURN(chirp, UINT32(rec->flags), STRING(argList), STRING(prm_getId(rec)), STRING(prm_getDesc(rec)),  UINTS8(len, data), END);
			return 0;
		}
	}
	return -1;	
}


int prm_format()
{
	flash_erase(PRM_FLASH_LOC, PRM_ALLOCATED_LEN);
	cprintf("All parameters have been erased and restored to their defaults!\n");
	g_dirty = true;
	return 0;
}

uint16_t prm_crc(const ParamRecord *rec)
{
	uint16_t crc;

	if (rec->len>PRM_MAX_LEN)
		return 0;

	crc = Chirp::calcCrc((uint8_t *)rec+2, rec->len+prm_getDataOffset(rec)-2); // +2, -2 because we don't include crc 

	// crc can't equal 0xffff
	if (crc==0xffff)
		crc = 0;

	return crc;
}

ParamRecord *prm_find(const char *id, uint8_t *buf=NULL)
{
	ParamRecord *rec, *begin, *end;

	if (buf)
	{
		begin =  (ParamRecord *)buf;
		end = (ParamRecord *)(buf+FLASH_SECTOR_SIZE);
	}
	else
	{
		begin =	(ParamRecord *)PRM_FLASH_LOC;
		end = (ParamRecord *)PRM_ENDREC;
	}
	
	for (rec=begin; rec->crc!=0xffff && rec<end; rec++)
	{
		if(strcmp(id, (char *)rec->data)==0)
			return rec;
	}
	return NULL;
}

uint32_t prm_nextFree()
{
	ParamRecord *rec;

	for (rec=(ParamRecord *)PRM_FLASH_LOC; rec->crc!=0xffff && rec<(ParamRecord *)PRM_ENDREC; rec++);

	if (rec>=(ParamRecord *)PRM_ENDREC)
		return NULL;
	return (uint32_t)rec; 
}

bool prm_verifyRecord(const ParamRecord *rec)
{	
	return prm_crc(rec)==rec->crc;
}

bool prm_verifyAll()
{
	ParamRecord *rec;

	for (rec=(ParamRecord *)PRM_FLASH_LOC; rec->crc!=0xffff && rec<(ParamRecord *)PRM_ENDREC; rec++)
	{
		if (prm_verifyRecord(rec)==false)
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
	ParamRecord *rec;
	uint8_t *buf;
	uint32_t offset;
	void *sector;
	int32_t res = 0;

	rec = prm_find(id);

	if (rec==NULL)
	{
		// Good god this is an ugly hack.  But, creating parameters should only be handled from within the firmware, so that the correct
		// description can be inserted.  There may be other parameters like this, such that when these parameters are lost, we want to resave,
		// in which case, we should formalize this hack somehow.
		if (strncmp(id, "Signature label", 15)==0)
		{
			char desc[100];
			sprintf(desc, "@c Signature_Labels Sets the label for objects that match signature%s.", id+15);
			prm_add(id, 0, desc, val[0], val+1, END);
			return 0;
		}
		return -1;
	}

	buf = (uint8_t *)malloc(FLASH_SECTOR_SIZE);
   	
	if (buf==NULL)
		return -2;

	sector = (void *)FLASH_SECTOR_MASK((uint32_t)rec);
	memcpy(buf, sector, FLASH_SECTOR_SIZE);

	rec = prm_find(id, buf);

	if (rec==NULL)
	{
		res = -1;
		goto end;
	}

	offset = prm_getDataOffset(rec);	
	if (memcmp((uint8_t *)rec+offset, val, valLen)==0)
		goto end;
	memcpy((uint8_t *)rec+offset, val, valLen);
	 	
	rec->len = valLen;
	rec->crc = prm_crc(rec);
	
	flash_erase((uint32_t)sector, FLASH_SECTOR_SIZE); 
	flash_program((uint32_t)sector, buf, FLASH_SECTOR_SIZE);


end:
	free(buf); 	
	return res;
}



int32_t prm_get(const char *id, ...)
{
	va_list args;
	ParamRecord *rec;
	int res;
	Shadow *shadow = prm_findShadow(id);

	if (shadow && shadow->data)
	{
		va_start(args, id);
		res = Chirp::vdeserialize(shadow->data, shadow->len, &args);
		va_end(args);
	}
	else
	{
		rec = prm_find(id);
		if (rec==NULL)
			return -1;
	
		va_start(args, id);
		res = Chirp::vdeserialize((uint8_t *)rec+prm_getDataOffset(rec), rec->len, &args);
		va_end(args);
	}
	 	
	return res;
}

int32_t prm_getChirp(const char *id, Chirp *chirp)
{
	ParamRecord *rec;

	Shadow *shadow = prm_findShadow(id);

	if (shadow && shadow->data)
		CRP_RETURN(chirp, UINTS8(shadow->len, shadow->data), END);
	else
	{
		rec = prm_find(id);
		if (rec==NULL)
			return -1;
	
		CRP_RETURN(chirp, UINTS8(rec->len, (uint8_t *)rec+prm_getDataOffset(rec)), END);
	}

	return 0;
}


int prm_add(const char *id, uint32_t flags, const char *desc, ...)
{
	char buf[PRM_MAX_LEN];
	int len;
    uint32_t freeLoc, offset=PRM_HEADER_LEN;
    va_list args;
	ParamRecord *rec = (ParamRecord *)buf;

	// if it already exists, 
	if (prm_find(id))
		return -2;

	memset((void *)rec, 0, PRM_MAX_LEN);

	strcpy((char *)rec+offset, id);
	offset += strlen(id) + 1;
	if (desc!=NULL)
	{
		strcpy((char *)rec+offset, desc);
		offset += strlen(desc) + 1;
	}
	else
	{
		*(char *)(rec+offset) = '\0';
	 	offset++;
	}

	// data section should be aligned to 4 bytes	
	ALIGN(offset, 4);

    va_start(args, desc);
    len = Chirp::vserialize(NULL, (uint8_t *)rec+offset, PRM_MAX_LEN-offset, &args);
    va_end(args);

	if (len<0)
		return -3;

	rec->flags = flags;
	rec->len = len;
	rec->crc = prm_crc(rec); 

	if ((freeLoc=prm_nextFree())==NULL)
		return -4;
	
	return flash_program(freeLoc, (uint8_t *)rec, len+prm_getDataOffset(rec));	
}

bool prm_dirty()
{
	// one-shot 
	bool res = g_dirty;
	g_dirty = false;

	return res;
}

int32_t prm_setDirty()
{
	g_dirty = true; // set dirty flag
	return 0;
}


int32_t prm_setShadowChirp(const char *id, const uint32_t &valLen, const uint8_t *val)
{
	Shadow *shadow = prm_findShadow(id);
    void *args[CRP_MAX_ARGS+1];	// +1 for the null arg

	if (shadow==NULL)
		return -1;

   	if (shadow->data==NULL)
	{
		shadow->data = (uint8_t *)malloc(valLen);
		shadow->len = valLen;
	}
	else if (valLen!=shadow->len)
		return -2;

	memcpy(shadow->data, val, valLen);

	if (shadow->callback)
	{
		Chirp::deserializeParse((uint8_t *)val, valLen, args);
		(*shadow->callback)(id, args[0]);  // only 1 arg for now, but we can add other cases by looking for the null arg
	}

	return 0;
}

int prm_setShadowCallback(const char *id, ShadowCallback callback)
{
	Shadow shadow, *pshadow;

	// can't create a shadow if it doesn't exist
	if (prm_find(id)==NULL)
		return -1;

	pshadow = prm_findShadow(id);
	if (pshadow) // shadow is already in the table
		pshadow->callback = callback;
	else // create new entry
	{
		uint32_t len = strlen(id)+1;
		shadow.id = (const char *)malloc(len);
		strcpy((char *)shadow.id, id);
		shadow.len = 0;
		shadow.data = NULL;
		shadow.callback = callback;

		g_shadowTable.push_back(shadow);
	}

	return 0;
}

