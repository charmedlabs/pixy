#ifndef _CONNCOMP_H
#define _CONNCOMP_H
#include "chirp.hpp"
#include "cblob.h"

#define RLS_MEMORY_SIZE     0x8000 // bytes
#define RLS_MEMORY          ((uint8_t *)SRAM0_LOC)
#define LUT_MEMORY_SIZE		0x10000 // bytes
#define LUT_MEMORY			((uint8_t *)SRAM0_LOC + SRAM0_SIZE-LUT_MEMORY_SIZE)  // +0x100 make room for prebuf and palette

int cc_init(Chirp *chirp);

int32_t cc_setModel(const uint8_t &model, const uint16_t &xoffset, const uint16_t &yoffset, const uint16_t &width, const uint16_t &height, Chirp *chirp=NULL);
int32_t cc_setMemory(const uint32_t &location, const uint32_t &len, const uint8_t *data);
int32_t cc_getRLSFrameChirp(Chirp *chirp);
int32_t cc_getRLSFrame(uint32_t *memory, uint8_t *lut, bool sync=true);

int32_t cc_getRLSCCChirp(Chirp *chirp);
int handleRL(CBlobAssembler *blobber, uint8_t model, int row, int startCol, int len);
int32_t cc_getMaxBlob(uint32_t *qvals, uint32_t numRls, int16_t *bdata);

#endif
