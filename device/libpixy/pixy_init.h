#ifndef PIXY_INIT_H
#define PIXY_INIT_H

#include "lpc_types.h"
#include "debug_frmwrk.h"
#include "debug.h"
#include "lpc43xx_adc.h"
#include "usb.h"
#include "usbcfg.h"
#include "usbcore.h"
#include "usbuser.h"
#include "ipc_mbx.h"
#include "chirpm0.h"
#include "chirpusb.h"
#include "pixyvals.h"

extern uint8_t *__Vectors;
#define STACK_GUARD           *(uint16_t *)(__Vectors - 0x600)
#define STACK_GUARD_WORD      0xABCD

void pixyInit(uint32_t slaveRomStart, const unsigned char slaveImage[], uint32_t imageSize);
void pixySimpleInit(void);

void cprintf(const char *format, ...);
void periodic();

extern Chirp *g_chirpUsb;
extern Chirp *g_chirpM0;

#endif
