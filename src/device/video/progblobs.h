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

#ifndef _PROGBLOBS_H
#define _PROGBLOBS_H

#include "exec.h"

#define SYNC_SERVO                 0x00ff
#define SYNC_CAM_BRIGHTNESS        0x00fe
#define SYNC_SET_LED               0x00fd

extern Program g_progBlobs;
extern bool g_ledSet;

uint32_t spiCallback(uint16_t *data, uint32_t len);

int blobsSetup();
int blobsLoop();

#endif
