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

#ifndef _FRAME_M0_H
#define _FRAME_M0_H

#include <cameravals.h>
#include <lpc43xx.h>

#define CAM_PORT 		(LPC_GPIO_PORT->PIN[1])
#define CAM_VSYNC() 	(CAM_PORT&0x1000)
#define CAM_HSYNC() 	(CAM_PORT&0x800)

int frame_init(void);
void skipLine(void);
void skipLines(uint32_t lines);

void callSyncM0(void);
void callSyncM1(void);
void grabM0R0(uint32_t xoffset, uint32_t yoffset, uint32_t xwidth, uint32_t ywidth, uint8_t *memory);
void grabM1R1(uint32_t xoffset, uint32_t yoffset, uint32_t xwidth, uint32_t ywidth, uint8_t *memory);
void grabM1R2(uint32_t xoffset, uint32_t yoffset, uint32_t xwidth, uint32_t ywidth, uint8_t *memory);
int32_t getFrame(uint8_t *type, uint32_t *memory, uint16_t *xoffset, uint16_t *yoffset, uint16_t *xwidth, uint16_t *ywidth);

#endif
