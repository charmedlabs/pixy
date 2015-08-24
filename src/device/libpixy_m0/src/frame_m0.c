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

#include "chirp.h"
#include "frame_m0.h"
#include "assembly.h"

#define CAM_PCLK_MASK   0x2000

#define ALIGN(v, n)  ((uint32_t)v&((n)-1) ? ((uint32_t)v&~((n)-1))+(n) : (uint32_t)v)

void vsync()
{
	int v = 0, h = 0;

	while(1)
	{
		h = 0;
		while(CAM_VSYNC()!=0);
		while(1) // vsync low
		{
			while(CAM_HSYNC()==0)
			{
				if (CAM_VSYNC()!=0)
					goto end;
			}
			while(CAM_HSYNC()!=0); //grab data
			h++;
		}
end:
		v++;
		//if (v%25==0)
			//printf("%d %d\n", v, h);
	}
}

_ASM_FUNC void syncM0(uint32_t *gpioIn, uint32_t clkMask)
{
_ASM_START
	_ASM(PUSH	{r4})

_ASM_LABEL(start)
	// This sequence can be extended to reduce probability of false phase detection.
	// This routine acts as a "sieve", only letting a specific phase through.  
	// In practice, 2 different phases separated by 1 clock are permitted through
	// which is acceptable-- 5ns in a 30ns period.  
	// If the pixel clock is shifted 1/2 a cpu clock period (or less), with respect to the CPU clock, 2 phases will match.  
	// If the pixel clock is perfectly in line with the cpu clock, 1 phase will match.  
	// Worst case will aways be 2 possible phases. 
	// It takes between 50 and 200 cpu clock cycles to complete.  	
	_ASM(LDR 	r2, [r0]) // high
	_ASM(NOP)
	_ASM(LDR 	r3, [r0]) // low
	_ASM(BICS	r2, r3)
	_ASM(LDR 	r3, [r0]) // high
	_ASM(ANDS	r3, r2)
	_ASM(LDR 	r2, [r0]) // low
	_ASM(LDR 	r4, [r0]) // high
	_ASM(BICS 	r4, r2)
	_ASM(LDR 	r2, [r0]) // low
	_ASM(BICS	r4, r2)		
	_ASM(LDR 	r2, [r0]) // high
	_ASM(ANDS	r4, r2)		
	_ASM(LDR 	r2, [r0]) // low
	
	_ASM(BICS	r4, r2)
	_ASM(ANDS	r4, r3)

	_ASM(TST	r4, r1)
	_ASM(BEQ	start)

	// in-phase begins here


	_ASM(POP   	{r4})
#ifdef KEIL
	_ASM(BX 	lr)
#endif
_ASM_END
}

_ASM_FUNC void syncM1(uint32_t *gpioIn, uint32_t clkMask)
{
	_ASM_START
	_ASM(PUSH	{r4})

_ASM_LABEL(startSyncM1)
	_ASM(LDR 	r2, [r0]) // high
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(LDR 	r3, [r0]) // low
	_ASM(BICS	r2, r3)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(LDR 	r3, [r0]) // high
	_ASM(ANDS	r3, r2)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(LDR 	r2, [r0]) // low
	_ASM(LDR 	r4, [r0]) // high
	_ASM(BICS 	r4, r2)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(LDR 	r2, [r0]) // low
	_ASM(BICS	r4, r2)		
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(LDR 	r2, [r0]) // high
	_ASM(ANDS	r4, r2)		
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(LDR 	r2, [r0]) // low
	
	_ASM(BICS	r4, r2)
	_ASM(ANDS	r4, r3)

	_ASM(TST		r4, r1)
	_ASM(NOP)		// an extra NOP makes us converge faster, worst case 400 cycles.  
	_ASM(NOP)
	_ASM(NOP)
	_ASM(BEQ		startSyncM1)

	// in-phase begins here

	_ASM(POP   	{r4})
#ifdef KEIL
	_ASM(BX 	lr)
#endif
	_ASM_END
}

_ASM_FUNC void lineM0(uint32_t *gpio, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
	_ASM_START
 	_ASM_IMPORT(callSyncM0)

#ifdef KEIL
	_ASM(PUSH	{r4-r5, lr})
#else
	_ASM(PUSH	{r4-r5})
#endif

	// add width to memory pointer so we can compare
	_ASM(ADDS	r3, r1)
	// generate hsync bit
	_ASM(MOVS	r4, #0x1)
	_ASM(LSLS	r4, #11)

	_ASM(PUSH	{r0-r3}) // save args
	_ASM(BL.W	callSyncM0) // get pixel sync
	_ASM(POP	{r0-r3})	// restore args
	   
	// pixel sync starts here

	// these nops are set us up for sampling hsync reliably
	_ASM(NOP) // 1
	_ASM(NOP) // 1

	// wait for hsync to go high
_ASM_LABEL(dest21)
	_ASM(LDR 	r5, [r0]) 	// 2
	_ASM(TST	r5, r4)		// 1
	_ASM(BEQ	dest21)		// 3

		// skip pixels
_ASM_LABEL(dest22)
	_ASM(SUBS	r2, #0x1)	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(BGE	dest22)		// 3

	// variable delay --- get correct phase for sampling

	_ASM(LDRB 	r2, [r0]) 	  // 0
	_ASM(STRB 	r2, [r1, #0x00])
	_ASM(NOP)
	_ASM(NOP)
	
	_ASM(LDRB 	r2, [r0]) 	  // 0
	_ASM(STRB 	r2, [r1, #0x01])
	_ASM(NOP)
	_ASM(NOP)

_ASM_LABEL(loop11)
	_ASM(LDRB 	r2, [r0]) 	  // 0
	_ASM(STRB 	r2, [r1, #0x2])

	_ASM(ADDS	r1, #0x03)
	_ASM(NOP)

	_ASM(LDRB 	r2, [r0])	  // 0
	_ASM(STRB 	r2, [r1, #0x0])

	_ASM(CMP		r1, r3)

	_ASM(LDRB 	r2, [r0])	  // -1
	_ASM(STRB 	r2, [r1, #0x1]) 

	_ASM(BLT		loop11)

	// wait for hsync to go low (end of line)
_ASM_LABEL(dest13)
	_ASM(LDR 	r5, [r0]) 	// 2
	_ASM(TST		r5, r4)		// 1
	_ASM(BNE		dest13)		// 3

#ifdef KEIL
	_ASM(POP		{r4-r5, pc})
#else
	_ASM(POP		{r4-r5})
#endif
	_ASM_END
}

_ASM_FUNC void lineM1R1(uint32_t *gpio, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
	_ASM_START
	_ASM_IMPORT(callSyncM1)

#ifdef KEIL
	_ASM(PUSH	{r4-r5, lr})
#else
	_ASM(PUSH	{r4-r5})
#endif

	// add width to memory pointer so we can compare
	_ASM(ADDS	r3, r1)
	// generate hsync bit
	_ASM(MOVS	r4, #0x1)
	_ASM(LSLS	r4, #11)

	_ASM(PUSH	{r0-r3}) // save args
	_ASM(BL.W	callSyncM1 )// get pixel sync
	_ASM(POP	{r0-r3})	// restore args
	   
	// pixel sync starts here

	// wait for hsync to go high
_ASM_LABEL(dest1)
	_ASM(LDR 	r5, [r0]) 	// 2
	_ASM(TST	r5, r4)		// 1
	_ASM(BEQ	dest1)		// 3

		// skip pixels
_ASM_LABEL(dest2)
	_ASM(SUBS	r2, #0x1)	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(BGE	dest2)		// 3

	// variable delay --- get correct phase for sampling
	_ASM(NOP)
	_ASM(NOP)

_ASM_LABEL(loop1)
	_ASM(LDRB 	r2, [r0]) 	  
	_ASM(STRB 	r2, [r1])
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(ADDS	r1, #0x01)
	_ASM(CMP	r1, r3)
	_ASM(BLT	loop1)

	// wait for hsync to go low (end of line)
_ASM_LABEL(dest3)
	_ASM(LDR 	r5, [r0]) 	// 2
	_ASM(TST	r5, r4)		// 1
	_ASM(BNE	dest3)		// 3

#ifdef KEIL
	_ASM(POP	{r4-r5, pc})
#else
	_ASM(POP	{r4-r5})
#endif
_ASM_END
}

_ASM_FUNC void lineM1R2(uint32_t *gpio, uint16_t *memory, uint32_t xoffset, uint32_t xwidth)
{
	_ASM_START
	_ASM_IMPORT(callSyncM1)

#ifdef KEIL
	_ASM(PUSH	{r4-r6, lr})
#else
	_ASM(PUSH	{r4-r6})
#endif

	// add width to memory pointer so we can compare
	_ASM(LSLS	r3, #1)
	_ASM(ADDS	r3, r1)
	// generate hsync bit
	_ASM(MOVS	r4, #0x1)
	_ASM(LSLS	r4, #11)

	_ASM(PUSH	{r0-r3}) // save args
	_ASM(BL.W	callSyncM1) // get pixel sync
	_ASM(POP	{r0-r3})	// restore args
	  
	// pixel sync starts here

_ASM_LABEL(dest7)
	_ASM(LDR 	r5, [r0]) 	// 2
	_ASM(TST	r5, r4)		// 1
	_ASM(BEQ	dest7)		// 3

		// skip pixels
_ASM_LABEL(dest8)
	_ASM(SUBS	r2, #0x1)	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(BGE	dest8)		// 3

	// variable delay --- get correct phase for sampling
	_ASM(NOP)
	_ASM(NOP)

_ASM_LABEL(loop3)
	_ASM(LDRB 	r2, [r0])
	_ASM(NOP)
	_ASM(NOP) 	  
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(LDRB 	r5, [r0]) 	  
	_ASM(NOP)
	_ASM(NOP) 	  
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)

	_ASM(LDRB 	r6, [r0]) 	  
	_ASM(ADDS   r6, r2)
	_ASM(STRH 	r6, [r1, #0x00])
	_ASM(NOP)
	_ASM(NOP) 	  
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)

	_ASM(LDRB 	r6, [r0]) 	  
	_ASM(ADDS   r6, r5)
	_ASM(STRH 	r6, [r1, #0x02])
	_ASM(NOP)
	_ASM(NOP)	  
	_ASM(ADDS	r1, #0x04)
	_ASM(CMP	r1, r3)
	_ASM(BLT	loop3)

		// wait for hsync to go low (end of line)
_ASM_LABEL(dest9)
	_ASM(LDR 	r5, [r0]) 	// 2
	_ASM(TST	r5, r4)		// 1
	_ASM(BNE	dest9)		// 3

#ifdef KEIL
	_ASM(POP	{r4-r6, pc})
#else
	_ASM(POP	{r4-r6})
#endif
_ASM_END
}

_ASM_FUNC void lineM1R2Merge(uint32_t *gpio, uint16_t *lineMemory, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
	_ASM_START
	_ASM_IMPORT(callSyncM1)

#ifdef KEIL
	_ASM(PUSH	{r4-r7, lr})
	_ASM(LDR	r4, [sp, #0x14])
#else
	_ASM(PUSH	{r4-r7})
	_ASM(LDR	r4, [sp, #0x28])
#endif

	// add width to memory pointer so we can compare
	_ASM(ADDS	r4, r2)
	// generate hsync bit
	_ASM(MOVS	r5, #0x1)
	_ASM(LSLS	r5, #11)

	_ASM(PUSH	{r0-r3}) // save args
	_ASM(BL.W	callSyncM1) // get pixel sync
	_ASM(POP	{r0-r3})	// restore args
	   
	// pixel sync starts here

		// wait for hsync to go high
_ASM_LABEL(dest4)
	_ASM(LDR 	r6, [r0]) 	// 2
	_ASM(TST	r6, r5)		// 1
	_ASM(BEQ	dest4)		// 3

		// skip pixels
_ASM_LABEL(dest5)
	_ASM(SUBS	r3, #0x1)	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(NOP)				 	// 1
	_ASM(BGE	dest5)		// 3

	// variable delay --- get correct phase for sampling
	_ASM(NOP)
	_ASM(NOP)

_ASM_LABEL(loop4)
	_ASM(LDRB 	r3, [r0]) // 0
	_ASM(LDRH	r6, [r1, #0x00])
	_ASM(ADDS   r6, r3)
	_ASM(NOP)
	_ASM(NOP)	  
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	
	_ASM(LDRB 	r3, [r0]) // 0
	_ASM(LDRH	r7, [r1, #0x02])
	_ASM(ADDS   r7, r3)
	_ASM(NOP)
	_ASM(NOP) 	  
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)

	_ASM(LDRB 	r3, [r0]) 	  // 0
	_ASM(ADDS   r6, r3)
	_ASM(LSRS   r6, #2)
	_ASM(STRB 	r6, [r2, #0x00])
	_ASM(NOP) 	
	_ASM(NOP)
	_ASM(NOP)  
	_ASM(NOP) 	
	_ASM(NOP)
	_ASM(NOP)  

	_ASM(LDRB 	r3, [r0]) 	 // 0 
	_ASM(ADDS   r7, r3)
	_ASM(LSRS   r7, #2)
	_ASM(STRB 	r7, [r2, #0x01])
	_ASM(ADDS   r1, #0x04)
	_ASM(ADDS	r2, #0x02)
	_ASM(CMP	r2, r4)
	_ASM(BLT	loop4)

	// wait for hsync to go low (end of line)
_ASM_LABEL(dest6)
	_ASM(LDR 	r6, [r0]) 	// 2
	_ASM(TST	r6, r5)		// 1
	_ASM(BNE	dest6)		// 3
	
#ifdef KEIL
	_ASM(POP	{r4-r7, pc})
#else
	_ASM(POP	{r4-r7})
#endif
_ASM_END
}

void skipLine()
{
	while(!CAM_HSYNC());
	while(CAM_HSYNC());
}

void skipLines(uint32_t lines)
{
	uint32_t line;

	// wait for remainder of frame to pass
	while(!CAM_VSYNC()); 
	// vsync asserted
	while(CAM_VSYNC());
	// skip lines
	for (line=0; line<lines; line++)
		skipLine();
}

void grabM0R0(uint32_t xoffset, uint32_t yoffset, uint32_t xwidth, uint32_t ywidth, uint8_t *memory)
{
	uint32_t line;

	xoffset >>= 1;
	yoffset &= ~1;

	skipLines(yoffset);
	for (line=0; line<ywidth; line++, memory+=xwidth)
		lineM0((uint32_t *)&CAM_PORT, memory, xoffset, xwidth); // wait, grab, wait
}

void grabM1R1(uint32_t xoffset, uint32_t yoffset, uint32_t xwidth, uint32_t ywidth, uint8_t *memory)
{
	uint32_t line;

	xoffset >>= 1;
	yoffset &= ~1;

	skipLines(yoffset);
	for (line=0; line<ywidth; line++, memory+=xwidth)
		lineM1R1((uint32_t *)&CAM_PORT, memory, xoffset, xwidth); // wait, grab, wait
}

void grabM1R2(uint32_t xoffset, uint32_t yoffset, uint32_t xwidth, uint32_t ywidth, uint8_t *memory)
{
	uint32_t line;
	uint16_t *lineStore = (uint16_t *)(memory + xwidth*ywidth + 16);
	lineStore = (uint16_t *)ALIGN(lineStore, 2);

	// clear line storage for 1 line
	for (line=0; line<xwidth; line++)
		lineStore[line] = 0;

	skipLines(yoffset*2);
	// grab 1 line to put us out of phase with the camera's internal vertical downsample (800 to 400 lines)
	// ie, we are going to downsample again from 400 to 200.  Because the bayer lines alternate
	// there tends to be little difference between line pairs bg and gr lines after downsampling.
	// Same logic applies horizontally as well, but we always skip a pixel pair in the line routine.  
	lineM1R2Merge((uint32_t *)&CAM_PORT, lineStore, memory, xoffset, xwidth); // wait, grab, wait
	memory += xwidth;
	for (line=0; line<ywidth; line+=2, memory+=xwidth*2)
	{
		// CAM_HSYNC is negated here
		lineM1R2((uint32_t *)&CAM_PORT, lineStore, xoffset, xwidth); // wait, grab, wait
		lineM1R2((uint32_t *)&CAM_PORT, lineStore+xwidth, xoffset, xwidth); // wait, grab, wait
		lineM1R2Merge((uint32_t *)&CAM_PORT, lineStore, memory, xoffset, xwidth); // wait, grab, wait
		lineM1R2Merge((uint32_t *)&CAM_PORT, lineStore+xwidth, memory+xwidth, xoffset, xwidth); // wait, grab, wait
	}					
}

void callSyncM0(void)
{
	syncM0((uint32_t *)&CAM_PORT, CAM_PCLK_MASK);
}

void callSyncM1(void)
{
	syncM1((uint32_t *)&CAM_PORT, CAM_PCLK_MASK);
}

int32_t getFrame(uint8_t *type, uint32_t *memory, uint16_t *xoffset, uint16_t *yoffset, uint16_t *xwidth, uint16_t *ywidth)
{
	//printf("M0: grab %d %d %d %d %d\n", *type, *xoffset, *yoffset, *xwidth, *ywidth);

	if (*type==CAM_GRAB_M0R0)
		grabM0R0(*xoffset, *yoffset, *xwidth, *ywidth, (uint8_t *)*memory);
	else if (*type==CAM_GRAB_M1R1)
		grabM1R1(*xoffset, *yoffset, *xwidth, *ywidth, (uint8_t *)*memory);
	else if (*type==CAM_GRAB_M1R2)
		grabM1R2(*xoffset, *yoffset, *xwidth, *ywidth, (uint8_t *)*memory);
	else
		return -1;

	return 0;
}




int frame_init(void)
{
	chirpSetProc("getFrame", (ProcPtr)getFrame);
		
	return 0;	
}
