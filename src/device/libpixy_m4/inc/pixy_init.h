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
#include "chirp.hpp"
#include "pixyvals.h"

#ifdef KEIL
extern uint8_t *__Vectors;
#define STACK_GUARD           *(uint16_t *)(__Vectors - 0x600)
#else
extern uint8_t *g_pfnVectors;
#define STACK_GUARD           *(uint16_t *)(g_pfnVectors - 0x600)
#endif
#define STACK_GUARD_WORD      0xABCD

#ifdef KEIL
void pixyInit(uint32_t slaveRomStart, const unsigned char slaveImage[], uint32_t imageSize);
#else
void pixyInit(void);
#endif
void pixySimpleInit(void);

void cprintf(const char *format, ...);
void periodic();

extern Chirp *g_chirpUsb;
extern Chirp *g_chirpM0;

#endif
