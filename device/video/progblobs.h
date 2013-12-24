#ifndef _PROGBLOBS_H
#define _PROGBLOBS_H

#include "exec.h"

extern Program g_progBlobs;

uint32_t spiCallback(uint16_t *data, uint32_t len);

int blobsSetup();
int blobsLoop();

#endif
