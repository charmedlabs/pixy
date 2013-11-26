#ifndef PIXY_INIT_H
#define PIXY_INIT_H

#include "lpc_types.h"
#include "debug_frmwrk.h"
#include "lpc43xx_adc.h"
#include "usb.h"
#include "usbcfg.h"
#include "usbcore.h"
#include "usbuser.h"
#include "ipc_mbx.h"
#include "chirpm0.h"
#include "chirpusb.h"
#include "pixyvals.h"

void pixyInit(uint32_t slaveRomStart, const unsigned char slaveImage[], uint32_t imageSize);
void pixySimpleInit(void);

void cprintf(const char *format, ...);

extern Chirp *g_chirpUsb;
extern Chirp *g_chirpM0;

#endif
