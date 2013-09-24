#ifndef LINKSM_H
#define LINKSM_H

#include "pixyvals.h"

#define SM_LOC                 (SRAM4_LOC+0x3000)
#define SM_SIZE                (SRAM4_SIZE-0x3000)
#define SM_BUFSIZE             (SM_SIZE-4)

// status
#define SM_STATUS_DATA_AVAIL   0x01

int linkSend(const uint8_t *data, uint32_t len, uint16_t timeoutMs);
int linkReceive(uint8_t *data, uint32_t len, uint16_t timeoutMs);

typedef struct
{
	uint16_t sendStatus;
	uint16_t recvStatus;

	uint8_t buf[SM_BUFSIZE];
}
SmMap;

#define SM_OBJECT       ((SmMap *)SM_LOC)
 
#endif
