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

#define PRM_MAX_LEN                 256
#define PRM_HEADER_LEN              8
#define PRM_DATA_LEN                (PRM_MAX_LEN-PRM_HEADER_LEN)
#define PRM_ALLOCATED_LEN           (FLASH_SECTOR_SIZE*8) // 8 sectors
#define PRM_FLASH_LOC               (FLASH_BEGIN + FLASH_SIZE - PRM_ALLOCATED_LEN)  // last sectors
#define PRM_ENDREC_OFFSET           ((PRM_ALLOCATED_LEN/PRM_MAX_LEN)*PRM_MAX_LEN)  // last sector
#define PRM_ENDREC                  (PRM_FLASH_LOC + PRM_ENDREC_OFFSET)  // last sector

// Matternet: This file is only here as a shell so PixyMon doesn't fail.

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


int prm_init(Chirp *chirp)
{
    chirp->registerModule(g_module);
    return 0;
}

int32_t prm_resetShadows()
{
    return -1;
}

int32_t prm_getInfo(const char *id, Chirp *chirp)
{
    return -1;
}

int32_t  prm_getAll(const uint16_t &index, Chirp *chirp)
{
    return -1;
}

int prm_format()
{
    return -1;
}

int32_t prm_setChirp(const char *id, const uint32_t &valLen, const uint8_t *val)
{
    return -1;
}

int32_t prm_getChirp(const char *id, Chirp *chirp)
{
    return -1;
}

int32_t prm_setDirty()
{
    return -1;
}

int32_t prm_setShadowChirp(const char *id, const uint32_t &valLen, const uint8_t *val)
{
    return -1;
}
