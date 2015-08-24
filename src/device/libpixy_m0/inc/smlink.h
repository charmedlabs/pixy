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

#ifndef LINKSM_H
#define LINKSM_H

#include "pixyvals.h"

#define SM_LOC                 (SRAM4_LOC+0x3c00)
#define SM_SIZE                (SRAM4_SIZE-0x3c00)
#define SM_BUFSIZE             (SM_SIZE-4)

// status
#define SM_STATUS_DATA_AVAIL   0x01

int linkSend(const uint8_t *data, uint32_t len, uint16_t timeoutMs);
int linkReceive(uint8_t *data, uint32_t len, uint16_t timeoutMs);

typedef struct
{
	volatile uint16_t sendStatus;
	volatile uint16_t recvStatus;

	volatile uint8_t buf[SM_BUFSIZE];
}
SmMap;

#define SM_OBJECT       ((SmMap *)SM_LOC)
 
#endif
