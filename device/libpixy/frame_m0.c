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

__asm void syncM0(uint32_t *gpioIn, uint32_t clkMask)
{
	PUSH	{r4}

start
	// This sequence can be extended to reduce probability of false phase detection.
	// This routine acts as a "sieve", only letting a specific phase through.  
	// In practice, 2 different phases separated by 1 clock are permitted through
	// which is acceptable-- 5ns in a 30ns period.  
	// If the pixel clock is shifted 1/2 a cpu clock period (or less), with respect to the CPU clock, 2 phases will match.  
	// If the pixel clock is perfectly in line with the cpu clock, 1 phase will match.  
	// Worst case will aways be 2 possible phases. 
	// It takes between 50 and 200 cpu clock cycles to complete.  	
	LDR 	r2, [r0] // high
	NOP
	LDR 	r3, [r0] // low
	BICS	r2, r3
	LDR 	r3, [r0] // high
	ANDS	r3, r2
	LDR 	r2, [r0] // low
	LDR 	r4, [r0] // high
	BICS 	r4, r2
	LDR 	r2, [r0] // low
	BICS	r4, r2		
	LDR 	r2, [r0] // high
	ANDS	r4, r2		
	LDR 	r2, [r0] // low
	
	BICS	r4, r2
	ANDS	r4, r3

	TST		r4, r1
	BEQ		start

	// in-phase begins here


	POP   	{r4}
	BX 		lr
}

__asm void syncM1(uint32_t *gpioIn, uint32_t clkMask)
{
	PUSH	{r4}

startSyncM1
	LDR 	r2, [r0] // high
	NOP
	NOP
	NOP
	NOP
	LDR 	r3, [r0] // low
	BICS	r2, r3
	NOP
	NOP
	NOP
	LDR 	r3, [r0] // high
	ANDS	r3, r2
	NOP
	NOP
	NOP
	LDR 	r2, [r0] // low
	LDR 	r4, [r0] // high
	BICS 	r4, r2
	NOP
	NOP
	NOP
	LDR 	r2, [r0] // low
	BICS	r4, r2		
	NOP
	NOP
	NOP
	LDR 	r2, [r0] // high
	ANDS	r4, r2		
	NOP
	NOP
	NOP
	LDR 	r2, [r0] // low
	
	BICS	r4, r2
	ANDS	r4, r3

	TST		r4, r1
	NOP		// an extra NOP makes us converge faster, worst case 400 cycles.  
	NOP
	NOP
	BEQ		startSyncM1

	// in-phase begins here


	POP   	{r4}
	BX 		lr
}

__asm void lineM0(uint32_t *gpio, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
		PRESERVE8
		IMPORT 	callSyncM0

		PUSH	{r4-r5, lr}

		// add width to memory pointer so we can compare
		ADDS	r3, r1
		// generate hsync bit
	  	MOVS	r4, #0x1
		LSLS	r4, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM0 // get pixel sync
		POP		{r0-r3}	// restore args
	   
	   	// pixel sync starts here

		// these nops are set us up for sampling hsync reliably
		NOP
		NOP

		// wait for hsync to go high
dest21	LDR 	r5, [r0] 	// 2
		TST		r5, r4		// 1
		BEQ		dest21		// 3

		// skip pixels
dest22	SUBS	r2, #0x1	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		BGE		dest22		// 3

		// variable delay --- get correct phase for sampling

		LDRB 	r2, [r0] 	  // 0
		STRB 	r2, [r1, #0x00]
		NOP
		NOP

		LDRB 	r2, [r0] 	  // 0
		STRB 	r2, [r1, #0x01]
		NOP
		NOP

loop11
		LDRB 	r2, [r0] 	  // 0
		STRB 	r2, [r1, #0x2]

		ADDS	r1, #0x03
		NOP

		LDRB 	r2, [r0]	  // 0
		STRB 	r2, [r1, #0x0]

		CMP		r1, r3

		LDRB 	r2, [r0]	  // -1
		STRB 	r2, [r1, #0x1] 

		BLT		loop11

		// wait for hsync to go low (end of line)
dest13	LDR 	r5, [r0] 	// 2
		TST		r5, r4		// 1
		BNE		dest13		// 3

		POP		{r4-r5, pc}
}

__asm void lineM1R1(uint32_t *gpio, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r4-r5, lr}

		// add width to memory pointer so we can compare
		ADDS	r3, r1
		// generate hsync bit
	  	MOVS	r4, #0x1
		LSLS	r4, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM1 // get pixel sync
		POP		{r0-r3}	// restore args
	   
	   	// pixel sync starts here

		// wait for hsync to go high
dest1	LDR 	r5, [r0] 	// 2
		TST		r5, r4		// 1
		BEQ		dest1		// 3

		// skip pixels
dest2	SUBS	r2, #0x1	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		BGE		dest2		// 3

		// variable delay --- get correct phase for sampling
		NOP
		NOP

loop1
		LDRB 	r2, [r0] 	  
		STRB 	r2, [r1]
		NOP
		NOP
		NOP
		ADDS	r1, #0x01
		CMP		r1, r3
		BLT		loop1

		// wait for hsync to go low (end of line)
dest3	LDR 	r5, [r0] 	// 2
		TST		r5, r4		// 1
		BNE		dest3		// 3

		POP		{r4-r5, pc}
}

__asm void lineM1R2(uint32_t *gpio, uint16_t *memory, uint32_t xoffset, uint32_t xwidth)
{
		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r4-r6, lr}

		// add width to memory pointer so we can compare
		LSLS	r3, #1
		ADDS	r3, r1
		// generate hsync bit
	  	MOVS	r4, #0x1
		LSLS	r4, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM1 // get pixel sync
		POP		{r0-r3}	// restore args
	   
	   	// pixel sync starts here

dest7	LDR 	r5, [r0] 	// 2
		TST		r5, r4		// 1
		BEQ		dest7		// 3

		// skip pixels
dest8	SUBS	r2, #0x1	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		BGE		dest8		// 3

		// variable delay --- get correct phase for sampling
		NOP
		NOP

loop3
		LDRB 	r2, [r0]
		NOP
		NOP 	  
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP

		LDRB 	r5, [r0] 	  
		NOP
		NOP 	  
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP

		LDRB 	r6, [r0] 	  
		ADDS    r6, r2
		STRH 	r6, [r1, #0x00]
		NOP
		NOP 	  
		NOP
		NOP
		NOP
		NOP
		NOP

		LDRB 	r6, [r0] 	  
		ADDS    r6, r5
		STRH 	r6, [r1, #0x02]
		NOP
		NOP 	  
		ADDS	r1, #0x04
		CMP		r1, r3
		BLT		loop3

		// wait for hsync to go low (end of line)
dest9	LDR 	r5, [r0] 	// 2
		TST		r5, r4		// 1
		BNE		dest9		// 3

		POP		{r4-r6, pc}
}

__asm void lineM1R2Merge(uint32_t *gpio, uint16_t *lineMemory, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r4-r7, lr}
		LDR		r4, [sp, #0x14]

		// add width to memory pointer so we can compare
		ADDS	r4, r2
		// generate hsync bit
	  	MOVS	r5, #0x1
		LSLS	r5, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM1 // get pixel sync
		POP		{r0-r3}	// restore args
	   
	   	// pixel sync starts here

		// wait for hsync to go high
dest4	LDR 	r6, [r0] 	// 2
		TST		r6, r5		// 1
		BEQ		dest4		// 3

		// skip pixels
dest5	SUBS	r3, #0x1	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		NOP				 	// 1
		BGE		dest5		// 3

		// variable delay --- get correct phase for sampling
		NOP
		NOP

loop4
		LDRB 	r3, [r0] // 0
		LDRH	r6, [r1, #0x00]
		ADDS    r6, r3
		NOP
		NOP 	  
		NOP
		NOP
		NOP
		NOP
		NOP

		LDRB 	r3, [r0] // 0
		LDRH	r7, [r1, #0x02]
		ADDS    r7, r3
		NOP
		NOP 	  
		NOP
		NOP
		NOP
		NOP
		NOP

		LDRB 	r3, [r0] 	  // 0
		ADDS    r6, r3
		LSRS    r6, #2
		STRB 	r6, [r2, #0x00]
		NOP 	
		NOP
		NOP  
		NOP 	
		NOP
		NOP  

		LDRB 	r3, [r0] 	 // 0 
		ADDS    r7, r3
		LSRS    r7, #2
		STRB 	r7, [r2, #0x01]
		ADDS    r1, #0x04
		ADDS	r2, #0x02
		CMP		r2, r4
		BLT		loop4

		// wait for hsync to go low (end of line)
dest6	LDR 	r6, [r0] 	// 2
		TST		r6, r5		// 1
		BNE		dest6		// 3

		POP		{r4-r7, pc}
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
