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

#include "debug_frmwrk.h"
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


void syncM0(uint32_t *gpioIn, uint32_t clkMask)
{
asm(".syntax unified");

	asm("PUSH	{r4}");

asm("start:");
	// This sequence can be extended to reduce probability of false phase detection.
	// This routine acts as a "sieve", only letting a specific phase through.  
	// In practice, 2 different phases separated by 1 clock are permitted through
	// which is acceptable-- 5ns in a 30ns period.  
	// If the pixel clock is shifted 1/2 a cpu clock period (or less), with respect to the CPU clock, 2 phases will match.  
	// If the pixel clock is perfectly in line with the cpu clock, 1 phase will match.  
	// Worst case will aways be 2 possible phases. 
	// It takes between 50 and 200 cpu clock cycles to complete.  	
	asm("LDR 	r2, [r0]"); // high
	asm("NOP");
	asm("LDR 	r3, [r0]"); // low
	asm("BICS	r2, r3");
	asm("LDR 	r3, [r0]"); // high
	asm("ANDS	r3, r2");
	asm("LDR 	r2, [r0]"); // low
	asm("LDR 	r4, [r0]"); // high
	asm("BICS 	r4, r2");
	asm("LDR 	r2, [r0]"); // low
	asm("BICS	r4, r2");
	asm("LDR 	r2, [r0]"); // high
	asm("ANDS	r4, r2");
	asm("LDR 	r2, [r0]"); // low
	
	asm("BICS	r4, r2");
	asm("ANDS	r4, r3");

	asm("TST	r4, r1");
	asm("BEQ	start");

	// in-phase begins here
	asm("POP   	{r4}");

	asm(".syntax divided");
}


void syncM1(uint32_t *gpioIn, uint32_t clkMask)
{
asm(".syntax unified");

	asm("PUSH	{r4}");

asm("startSyncM1:");
	asm("LDR 	r2, [r0]"); // high
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("LDR 	r3, [r0]"); // low
	asm("BICS	r2, r3");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("LDR 	r3, [r0]"); // high
	asm("ANDS	r3, r2");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("LDR 	r2, [r0]"); // low
	asm("LDR 	r4, [r0]"); // high
	asm("BICS 	r4, r2");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("LDR 	r2, [r0]"); // low
	asm("BICS	r4, r2");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("LDR 	r2, [r0]"); // high
	asm("ANDS	r4, r2");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("LDR 	r2, [r0]"); // low
	
	asm("BICS	r4, r2");
	asm("ANDS	r4, r3");

	asm("TST		r4, r1");
	asm("NOP");		// an extra NOP makes us converge faster, worst case 400 cycles.
	asm("NOP");
	asm("NOP");
	asm("BEQ		startSyncM1");

	// in-phase begins here


	asm("POP   	{r4}");

	asm(".syntax divided");
}


void lineM0(uint32_t *gpio, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
//	asm("PRESERVE8");
//	asm("IMPORT callSyncM0");
asm(".syntax unified");

	asm("PUSH	{r4-r5}");

	// add width to memory pointer so we can compare
	asm("ADDS	r3, r1");
	// generate hsync bit
	asm("MOVS	r4, #0x1");
	asm("LSLS	r4, #11");

	asm("PUSH	{r0-r3}");	 	// save args
	asm("BL		callSyncM0");	// get pixel sync
	asm("POP	{r0-r3}");		// restore args
	   
	// pixel sync starts here

    // these nops are set us up for sampling hsync reliably
	asm("NOP");
	asm("NOP");

	// wait for hsync to go high
asm("dest21:");
    asm("LDR 	r5, [r0]");	 	// 2
	asm("TST	r5, r4");			// 1
	asm("BEQ	dest21");			// 3

		// skip pixels
asm("dest22:");
	asm("SUBS	r2, #0x1");	// 1
    asm("NOP");				// 1
    asm("NOP");				// 1
    asm("NOP");				// 1
    asm("NOP");				// 1
    asm("NOP");				// 1
    asm("NOP");				// 1
    asm("NOP");				// 1
    asm("NOP");				// 1
    asm("BGE	dest22");	// 3

	// variable delay --- get correct phase for sampling

    asm("LDRB 	r2, [r0]");	 	  // 0
    asm("STRB 	r2, [r1, #0x00]");
    asm("NOP");
    asm("NOP");

    asm("LDRB 	r2, [r0]");	 	  // 0
    asm("STRB 	r2, [r1, #0x01]");
    asm("NOP");
    asm("NOP");

asm("loop11:");
	asm("LDRB 	r2, [r0]"); 	  // 0
	asm("STRB 	r2, [r1, #0x2]");

	asm("ADDS	r1, #0x03");
	asm("NOP");

	asm("LDRB 	r2, [r0]");	  // 0
	asm("STRB 	r2, [r1, #0x0]");

	asm("CMP	r1, r3");

	asm("LDRB 	r2, [r0]");	  // -1
	asm("STRB 	r2, [r1, #0x1]");

	asm("BLT	loop11");

	// wait for hsync to go low (end of line)
asm("dest13:");
    asm("LDR 	r5, [r0]"); 	// 2
	asm("TST	r5, r4");		// 1
	asm("BNE	dest13");		// 3

	asm("POP	{r4-r5}");

	asm(".syntax divided");
}


void lineM1R1(uint32_t *gpio, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
//	asm("PRESERVE8");
//	asm("IMPORT	callSyncM1");
asm(".syntax unified");

	asm("PUSH	{r4-r5}");

	// add width to memory pointer so we can compare
	asm("ADDS	r3, r1");
	// generate hsync bit
	asm("MOVS	r4, #0x1");
	asm("LSLS	r4, #11");

	asm("PUSH	{r0-r3}"); // save args
	asm("BL 	callSyncM1"); // get pixel sync
	asm("POP	{r0-r3}");	// restore args
	   
	// pixel sync starts here

	// wait for hsync to go high
asm("dest1:");
    asm("LDR	r5, [r0]"); // 2
	asm("TST	r5, r4");	// 1
	asm("BEQ	dest1");	// 3

	// skip pixels
asm("dest2:");
	asm("SUBS	r2, #0x1");	// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("BGE	dest2");		// 3

	// variable delay --- get correct phase for sampling
	asm("NOP");
	asm("NOP");

asm("loop1:");
	asm("LDRB 	r2, [r0]");
	asm("STRB 	r2, [r1]");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("ADDS	r1, #0x01");
	asm("CMP	r1, r3");
	asm("BLT	loop1");

	// wait for hsync to go low (end of line)
asm("dest3:");
    asm("LDR 	r5, [r0]"); 	// 2
	asm("TST	r5, r4");		// 1
	asm("BNE	dest3");		// 3

	asm("POP	{r4-r5}");

	asm(".syntax divided");
}


void lineM1R2(uint32_t *gpio, uint16_t *memory, uint32_t xoffset, uint32_t xwidth)
{
//	asm("PRESERVE8");
//	asm("IMPORT callSyncM1");
asm(".syntax unified");

	asm("PUSH	{r4-r6}");

	// add width to memory pointer so we can compare
	asm("LSLS	r3, #1");
	asm("ADDS	r3, r1");
	// generate hsync bit
	asm("MOVS	r4, #0x1");
	asm("LSLS	r4, #11");

	asm("PUSH	{r0-r3}"); // save args
	asm("BL		callSyncM1"); // get pixel sync
	asm("POP	{r0-r3}");	// restore args
	   
	// pixel sync starts here
asm("dest7:");
   asm("LDR 	r5, [r0]"); // 2
   asm("TST		r5, r4");	// 1
   asm("BEQ		dest7");	// 3

   // skip pixels
asm("dest8:");
    asm("SUBS	r2, #0x1");	// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("NOP");				// 1
	asm("BGE	dest8");		// 3

	// variable delay --- get correct phase for sampling
	asm("NOP");
	asm("NOP");

asm("loop3:");
	asm("LDRB 	r2, [r0]");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");

	asm("LDRB 	r5, [r0]");
    asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");

	asm("LDRB 	r6, [r0]");
	asm("ADDS   r6, r2");
	asm("STRH 	r6, [r1, #0x00]");
    asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");

	asm("LDRB 	r6, [r0]");
	asm("ADDS   r6, r5");
	asm("STRH 	r6, [r1, #0x02]");
	asm("NOP");
	asm("NOP");
	asm("ADDS	r1, #0x04");
	asm("CMP	r1, r3");
	asm("BLT	loop3");

		// wait for hsync to go low (end of line)
asm("dest9:");
	asm("LDR 	r5, [r0]"); 	// 2
	asm("TST	r5, r4");		// 1
	asm("BNE	dest9");		// 3

	asm("POP	{r4-r6}");

	asm(".syntax divided");
}


void lineM1R2Merge(uint32_t *gpio, uint16_t *lineMemory, uint8_t *memory, uint32_t xoffset, uint32_t xwidth)
{
//	asm("PRESERVE8");
//	asm("IMPORT callSyncM1");
asm(".syntax unified");

	asm("PUSH	{r4-r7}");
	asm("LDR	r4, [sp, #0x28]"); // *** keil

   	// add width to memory pointer so we can compare
	asm("ADDS	r4, r2");
	// generate hsync bit
	asm("MOVS	r5, #0x1");
	asm("LSLS	r5, #11");

	asm("PUSH	{r0-r3}"); // save args
	asm("BL 	callSyncM1"); // get pixel sync
	asm("POP	{r0-r3}");	// restore args
	   
	// pixel sync starts here

	// wait for hsync to go high
asm("dest4:");
	asm("LDR 	r6, [r0]"); 	// 2
	asm("TST	r6, r5");		// 1
	asm("BEQ	dest4");		// 3

		// skip pixels
asm("dest5:");
	asm("SUBS	r3, #0x1");	    // 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("NOP");				 	// 1
	asm("BGE	dest5");		// 3

	// variable delay --- get correct phase for sampling
	asm("NOP");
	asm("NOP");

asm("loop4:");
	asm("LDRB 	r3, [r0]"); // 0
	asm("LDRH	r6, [r1, #0x00]");
	asm("ADDS   r6, r3");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");

	asm("LDRB 	r3, [r0]"); // 0
	asm("LDRH	r7, [r1, #0x02]");
	asm("ADDS   r7, r3");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");

	asm("LDRB	r3, [r0]"); 	  // 0
	asm("ADDS   r6, r3");
	asm("LSRS   r6, #2");
	asm("STRB 	r6, [r2, #0x00]");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");

	asm("LDRB	r3, [r0]"); 	 // 0
	asm("ADDS	r7, r3");
	asm("LSRS	r7, #2");
	asm("STRB	r7, [r2, #0x01]");
	asm("ADDS	r1, #0x04");
	asm("ADDS	r2, #0x02");
	asm("CMP	r2, r4");
	asm("BLT	loop4");

	// wait for hsync to go low (end of line)
asm("dest6:");
	asm("LDR	r6, [r0]"); 	// 2
	asm("TST	r6, r5");		// 1
	asm("BNE	dest6");		// 3

	asm("POP	{r4-r7}");

	asm(".syntax divided");
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
		if (line<CAM_RES2_HEIGHT-2)
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
