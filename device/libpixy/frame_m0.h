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
