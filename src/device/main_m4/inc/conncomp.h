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

#ifndef _CONNCOMP_H
#define _CONNCOMP_H
#include "chirp.hpp"
#include <blob.h>
#include "blobs.h"

#define LUT_MEMORY          ((uint8_t *)SRAM1_LOC + SRAM1_SIZE - CL_LUT_SIZE)

int cc_init(Chirp *chirp);

int32_t cc_setSigRegion(const uint32_t &type, const uint8_t &signum, const uint16_t &xoffset, const uint16_t &yoffset, const uint16_t &width, const uint16_t &height, Chirp *chirp=NULL);
int32_t cc_setSigPoint(const uint32_t &type, const uint8_t &signum, const uint16_t &x, const uint16_t &y, Chirp *chirp=NULL);
int32_t cc_clearSig(const uint8_t &signum, Chirp *chirp=NULL);
int32_t cc_clearAllSig(Chirp *chirp=NULL);
int32_t cc_setLabel(const uint32_t &signum, const char *label, Chirp *chirp=NULL);
int32_t cc_setMemory(const uint32_t &location, const uint32_t &len, const uint8_t *data);
int32_t cc_getRLSFrameChirp(Chirp *chirp);
int32_t cc_getRLSFrameChirpFlags(Chirp *chirp, uint8_t renderFlags=RENDER_FLAG_FLUSH);
int32_t cc_getRLSFrame(uint8_t *memory, uint8_t *lut, bool sync=true);
int cc_sendBlobs(Chirp *chirp, const BlobA *blobs, uint32_t len, uint8_t renderFlags=RENDER_FLAG_FLUSH);
int cc_sendBlobs(Chirp *chirp, const BlobA *blobs, uint32_t len, const BlobB *ccBlobs, uint32_t ccLen, uint8_t renderFlags=RENDER_FLAG_FLUSH);
int cc_loadLut(void);
void cc_sendPoints(Points &points, uint16_t width, uint16_t height, Chirp *chirp, uint8_t renderFlags=RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH);

void cc_loadParams(void);

void cc_setLED();

extern Qqueue *g_qqueue;
extern Blobs *g_blobs;
extern const uint32_t g_colors[];
extern uint16_t g_ledBrightness;

#endif
