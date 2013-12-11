#include <debug.h>
#include <chirp.h>
#include <cycletimer.h>
#include <pixyvals.h>
#include <cameravals.h>

#define CAM_PORT 		(LPC_GPIO_PORT->PIN[1])
#define CAM_VSYNC() 	(CAM_PORT&0x1000)
#define CAM_HSYNC() 	(CAM_PORT&0x800)
#define CAM_PCLK_MASK   0x2000

#define ALIGN(v, n)  ((uint32_t)v&((n)-1) ? ((uint32_t)v&~((n)-1))+(n) : (uint32_t)v)

uint8_t *g_logLut = NULL;

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
		if (v%25==0)
			printf("%d %d\n", v, h);
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


#if 0

// assemble blue-green words to look like this
// 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0
//                                                  B  B  B  B  B  G G G G G
__asm void lineProcessedRL0(uint32_t *gpio, uint16_t *memory, uint32_t width)
{ 
		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r4-r5, lr}

		// add width to memory pointer so we can compare
		ADDS	r2, r1
		// generate hsync bit
	  	MOVS	r5, #0x1
		LSLS	r5, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM1 // get pixel sync
		POP		{r0-r3}	// restore args
	   
	   	// pixel sync starts here

		// wait for hsync to go high
dest10	LDR 	r3, [r0] 	// 2
		TST		r3, r5		// 1
		BEQ		dest10		// 3

		// variable delay --- get correct phase for sampling
		NOP
		NOP

#if 0
loop5
		LDRB 	r3, [r0] 	  
		STRB 	r3, [r1]
		NOP
		NOP
		NOP
		ADDS	r1, #0x01
		CMP		r1, r2
		BLT		loop5
#else
loop5
		LDRB 	r3, [r0] // blue
		LSRS    r3, #3
		LSLS    r3, #10
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP

		LDRB 	r4, [r0] // green 	  
		LSRS    r4, #3
		LSLS    r4, #5
		ORRS	r3, r4
		STRH    r3,	[r1] // store blue/green
		ADDS    r1, #0x02
		CMP		r1, r2
		BLT		loop5

#endif
		// wait for hsync to go low (end of line)
dest11	LDR 	r3, [r0] 	// 2
		TST		r3, r5		// 1
		BNE		dest11		// 3

		POP		{r4-r5, pc}
}

__asm uint16_t *lineProcessedRL1(uint32_t *gpio, uint16_t *memory, uint8_t *lut, uint16_t *linestore, uint32_t width)
{
// r0: gpio
// r1: q memory
// r2: lut
// r3: prev line
// r4: col 
// r5: scratch
// r6: scratch
// r7: prev m val
		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r4-r7, lr}
		LDR		r4, [sp, #0x14]

		// add width to memory pointer so we can compare
		ADDS	r4, r3
	   	MOV		r8, r4
		// generate hsync bit
	  	MOVS	r5, #0x1
		LSLS	r5, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM1 // get pixel sync
		POP		{r0-r3}	// restore args
	   
	   	// pixel sync starts here
		
		// wait for hsync to go high
dest12	LDR 	r6, [r0] 	// 2
		TST		r6, r5		// 1
		BEQ		dest12		// 3

		// variable delay --- get correct phase for sampling
	    NOP
		//(borrow NOP below)
		// skip green pixel
		MOVS    r5, #0x00 // clear r5 (which will be copied to r7) This will force a write of a q val 
		MOVS	r4, #0x00 // clear col (col is numbered 1 to 320)
// 2
loop6	MOV		r7, r5 // copy lut val (lutPrev)
		MOV		r5, r8
		CMP		r3, r5
		BGE		dest30
		NOP
		NOP
		NOP
		NOP
		NOP
// 9
loop7	
		LDRH	r5, [r3] // load blue green val
// 2
		LDRB 	r6, [r0] // load red pixel
		LSRS    r6, #3	 // shift into place (5 bits of red)
		ORRS	r5, r6 // form 15-bit lut index
		ADDS    r3, #0x02 // inc prev line pointer
		ADDS	r4, #0x01 // inc col
		NOP
		LDRB	r5, [r2, r5] // load lut val
		EORS  	r7, r5 // compare with previous	lut val
// 10
		BEQ	   	loop6 // if lut vals have haven't changed proceed (else store q val)
// 1, 3
		// calc, store q val
		LSLS	r5, #0x09
		ORRS	r5, r4 // make q val
		STRH	r5, [r1] // write q val
		ADDS	r1, #0x02 // inc q mem
		MOV		r7, r5 // copy lut val (qPrev)
// 6

		MOV 	r5, r8 // bring in end of row compare val
		CMP		r3, r5
		BLT		loop7
// 5
dest30
	  	MOVS	r5, #0x1
		LSLS	r5, #11
dest20	LDR 	r6, [r0] 	// 2
		TST		r6, r5		// 1
		BNE		dest20		// 3

		MOV     r0, r1 // move result
		POP		{r4-r7, pc}
} 
#endif

__asm void lineProcessedRL0A(uint32_t *gpio, uint8_t *memory, uint32_t width) // width in bytes
{ 
		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r4-r5, lr}

		// add width to memory pointer so we can compare
		ADDS	r2, r1
		// generate hsync bit
	  	MOVS	r5, #0x1
		LSLS	r5, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM1 // get pixel sync
		POP		{r0-r3}	// restore args
	   
	   	// pixel sync starts here

		// wait for hsync to go high
dest10A	LDR 	r3, [r0] 	// 2
		TST		r3, r5		// 1
		BEQ		dest10A		// 3

		// variable delay --- get correct phase for sampling
		NOP
		NOP

loop5A
		LDRB 	r3, [r0] // blue
		// cycle
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

		LDRB 	r4, [r0] // green 	 
		// cycle 
		SUBS	r3, r4   // blue-green
		ASRS    r3, #1
		STRB    r3,	[r1] // store blue-green
		// cycle 
		ADDS    r1, #0x01
		CMP		r1, r2
		NOP
		BLT		loop5A

		// wait for hsync to go low (end of line)
dest11A	LDR 	r3, [r0] 	// 2
		TST		r3, r5		// 1
		BNE		dest11A		// 3

		POP		{r4-r5, pc}
}

//#define RLTEST

__asm uint32_t *lineProcessedRL1A(uint32_t *gpio, uint32_t *memory, uint8_t *lut, uint8_t *linestore, uint32_t width, uint8_t *shiftLut, uint32_t *copyLoc) // width in bytes
{
// The code below does the following---
// -- maintain pixel sync, read red and green pixels
// -- create r-g, b-g index and look up value in lut
// -- filter out noise within the line.  An on pixel surrounded by off pixels will be ignored.
//    An off pixel surrounded by on pixels will be ignored.
// -- generate hue line sum	and pseudo average
// -- generate run-length segments
// 
// Notes:
// Run-length segments are 1 pixel larger than actual, and the last hue line value is added twice in the sum.
// Spurious noise pixels within a run-length segment present a problem.  When this happens the last hue line value 
// is added to the sum to keep things unbiased.  ie, think of the case where a run-length consists of half 
// spurious noise pixels.  We don't want the noise to affect the average--- only the pixels that agree with
// the model number for that run-length.
// All pixels are read and used--- the opponent color space (r-g, b-g) works well with the bayer pattern.
// After the red pixel is read, about 12 cycles are used to create the index and look it up.  When the q val is 
// created and written it takes about 24 cycles, so a green/red pixel pair is skipped, but the green pixel is 
// grabbed and put in r5 so that it can be used when we resume. 
// A shift lut is provided-- it's 321 entries containing the log2 of the index, so it's basically a log function
// that helps us fit the hue line sum in the q value.  This value indicates how many bits to shift off to 
// the right to reduce the size of the sum.  That number is also stored in the q val so that it can be 
// reversed on the m4 side and a real division can take place.   
//
// r0: gpio	register
// r1: scratch 
// r2: lut
// r3: prev line
// r4: column 
// r5: scratch
// r6: scratch
// r7: prev model
// r8: sum
// r9: ending column
// r10: beginning column of run-length
// r11: last lut val
// r12:	q memory 

#ifndef RLTEST
		MACRO 
$lx		RED		
$lx		LDRB 	r6, [r0] // load red pixel 
		// cycle
		MEND

		MACRO 
$lx		GREEN		
$lx		LDRB 	r5, [r0] // load green pixel 
		// cycle
		MEND
#else // RLTEST
		MACRO 
$lx		RED
$lx		LDRB 	r6, [r0, r4] // load red pixel 
		// cycle
		MEND

		MACRO 
$lx		GREEN		
$lx		LDRB 	r5, [r0, r4] // load green pixel 
		ADDS	r4, #1		
		// cycle
		MEND
#endif	// RLTEST

		MACRO // create index, lookup, inc col, extract model
$lx		LEXT	$rx
$lx		RED
		// cycle
		SUBS	r6, r5   // red-green
		ASRS	r6, #1	 // reduce 9 to 8 bits arithmetically
		LSLS	r6, #24  // shift red-green and get rid of higher-order bits
		LSRS	r6, #16  // shift red-green back, make it the higher 8 bits of the index
		LDRB	r5, [r3, r4] // load blue-green val
		// cycle
		ORRS	r5, r6   // form 16-bit index
		LDRB	r1, [r2, r5] // load lut val
		// cycle
		ADDS 	r4, #1 // inc col 
		// *** PIXEL SYNC
		GREEN
		// cycle
		LSLS	$rx, r1, #29 // knock off msb's
		LSRS	$rx, #29 // extract model, put in rx
		MEND

		MACRO // check for end of line
$lx		EOL_CHECK
$lx		CMP 	r4, r9
		BGE		eol
	   	MEND

		// q val:
		// | 4 bits    | 7 bits		 | 9 bits | 9 bits    | 3 bits |
		// | shift val | shifted sum | length | begin col | model  |
		MACRO // create q val
$lx		QVAL
		MOV 	r5, r10 // r5 gets beginning column 
		LSLS 	r6, r5, #3 // r6 gets shifted beginning column
		ORRS	r6, r7 // add model
		SUBS  	r1, r4, r5 // r1 gets length
		LSLS 	r5, r1, #12 // r5 gets shifted length
		ORRS 	r6, r5 // combine
		// *** PIXEL SYNC (red)
		LDR		r5, [sp, #0x24] // bring in shift lut 
		// cycle
		LDRB 	r1, [r1, r5] // look up	number of shifts (log2)
		// cycle
		MOV 	r5, r8 // bring in sum
		LSRS 	r5, r1 // shift it the required amount-- biased by 3 
		LSLS	r5, #21 // make room for shifted sum
		ORRS 	r6, r5 // combine shifted sum
		LSLS 	r1, #28 // shift shift-val
		ORRS	r6, r1 // save shift number
		MOV 	r1, r12 // bring in q mem
		MOVS 	r7, #4
		// *** PIXEL SYNC (green)
		GREEN
		// cycle
		STR 	r6, [r1] // store q val
		// cycle
		ADD  	r12, r7 // increment q mem
		MEND

		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r1-r7, lr}
		// bring in ending column
		LDR		r4, [sp, #0x20]
	   	MOV		r9, r4
	  	MOVS	r5, #0x1
		LSLS	r5, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM1 // get pixel sync
		POP		{r0-r3}	// restore args

	   	// pixel sync starts here
		
		// wait for hsync to go high
dest12A	LDR 	r6, [r0] 	// 2
		TST		r6, r5		// 1
		BEQ		dest12A		// 3

		// variable delay --- get correct phase for sampling
		MOV		r12, r1 // save q memory
		MOVS	r4, #0 // clear column value

		// *** PIXEL SYNC (start reading pixels)
		GREEN
		// cycle
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
zero0	MOVS	r6, #0 
		MOV		r8, r6	// clear sum (so we don't think we have an outstanding segment)
		EOL_CHECK
		// cycle
		// *** PIXEL SYNC (check for nonzero lut value)
zero1	LEXT 	r7
		// cycle
		// cycle
		// cycle
		CMP 	r7, #0
		BEQ 	zero0 
		MOV		r10, r4 // save start column
		ADD		r8, r1 // add to sum
		EOL_CHECK
		// cycle
		NOP
		NOP
		// *** PIXEL SYNC (check nonzero value for consistency)
		LEXT 	r6
		// cycle
		// cycle
		// cycle
		CMP		r6, r7
		BNE		zero0
		NOP
		NOP
one		MOV		r11, r1	// save last lut val
		ADD		r8, r1 // add to sum
		EOL_CHECK
		// cycle
		// *** PIXEL SYNC (run-length segment)
		LEXT 	r6
		// cycle
		// cycle
		// cycle
		CMP		r6, r7
		BEQ		one
		ADD		r8, r11 // need to add something-- use last lut val
		EOL_CHECK
		// cycle
		NOP
		NOP
		NOP
		// *** PIXEL SYNC (1st pixel not equal)
		LEXT 	r6
		// cycle
		// cycle
		// cycle
		CMP		r6, r7
		BEQ		one	
		// 2nd pixel not equal--- run length is done
		QVAL
		// cycle
		// cycle
		// cycle
		// cycle
		MOVS	r6, #0
		MOV		r8, r6 // clear sum
		ADDS	r4, #1 // add column
		NOP
		B 		zero1
			
eol	    
		// check r8 for unfinished q val
		MOVS	r6, #0
		CMP		r8, r6
		BEQ		eol0
		QVAL		

	   	// wait for hsync to go low
eol0	MOVS	r5, #0x1
		LSLS	r5, #11
dest20A	LDR 	r6, [r0] 	// 2
		TST		r6, r5		// 1
		BNE		dest20A		// 3

	    // we have approx 1800 cycles to do something here
		LDR		r0, [sp, #0x28] // bring in qq pointer (assume we have enough space (256 bytes) 
		LDR		r1, [sp] // bring in original q memory location 
lcpy	CMP		r12, r1	  // 1 if pointers are equal, we're done
		BEQ		ecpy	  // 1 exit

		LDR		r3, [r1]  // 2 copy (read)
		STR		r3, [r1]  // 2 copy (write)
		ADDS	r0, #4	  // 1 inc
		ADDS 	r1, #4	  // 1 inc
		B		lcpy	  // 3

ecpy	MOV     r0, r12 // return end of q mem
		POP		{r1-r7, pc}
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
	printf("M0: grab %d %d %d %d %d\n", *type, *xoffset, *yoffset, *xwidth, *ywidth);

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

uint8_t intLog(int i)
{
	return 0;
}

void createLogLut(void)
{
	int i;
	
	for (i=0; i<CAM_RES2_WIDTH; i++)
		g_logLut[i] = intLog(i) + 3;
}

#ifdef RLTEST
uint8_t bgData[] = 
{
0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12 
};

uint32_t rgData[] = 
{
0x08a008a0, 0x08a008a0, 0x08a008a0, 0x08a008a0, 0x08a008a0, 0x08a008a0
};
#endif

int32_t getRLSFrame(uint32_t *memory, uint32_t *size /*bytes*/, uint32_t *lut)
{
	uint8_t *lut2 = (uint8_t *)*lut;
	uint32_t *memory2 = (uint32_t *)*memory;
	uint32_t line;
	uint32_t *memory2Orig = memory2; 
	uint8_t *lineStore = (uint8_t *)memory2 + *size-CAM_RES2_WIDTH*2-4;
	uint8_t *logLut = lineStore + CAM_RES2_WIDTH + 4;
	 
	if (g_logLut!=logLut)
	{
		g_logLut = logLut; 
	 	createLogLut();
	}

	skipLines(0);
	for (line=0; line<CAM_RES2_HEIGHT; line++)
	{
		// mark beginning of this row (column 0 = 0)
		// column 0 is a symbolic column to the left of column 1.  (column 1 is the first real column of pixels)
		// (there is an implied end of line before the begin of line) 
		*memory2++ = 0x0000; 
		lineProcessedRL0A((uint32_t *)&CAM_PORT, lineStore, CAM_RES2_WIDTH); 
#ifndef RLTEST
		memory2 = lineProcessedRL1A((uint32_t *)&CAM_PORT, memory2, lut2, lineStore, CAM_RES2_WIDTH, g_logLut, (uint32_t *)0xdeadbeef);
#else
		memory2 = lineProcessedRL1A(rgData, memory2, lut2, (uint8_t *)bgData, CAM_RES2_WIDTH, g_logLut);
#endif
		if ((uint32_t *)lineStore-memory2<CAM_RES2_WIDTH/5)	// width/5 because that's the worst case with noise filtering
		{
#ifndef RLTEST
			CRP_RETURN(UINT32(memory2 - memory2Orig), END); 
			return -1; 
#else
			return memory2 - memory2Orig;
#endif
		}
	}
#ifndef RLTEST
	CRP_RETURN(UINT32(memory2 - memory2Orig), END); 
	return 0;
#else
	return memory2 - memory2Orig;
#endif
}

#if 0 // this would initialize our ChirpProcs, if we had any--- uncomment when we add some
void chirpInit(void)
{
}
#endif

int main(void)
{
	//CTIMER_DECLARE();
#if 0
	uint32_t size = SRAM0_SIZE/2;
	uint32_t memory = SRAM0_LOC;
	uint32_t lut = SRAM0_LOC;
		
 	while(1)
 		getRLSFrame(&memory, &size, &lut); 
#endif
	//printf("M0 start\n");

	chirpOpen();
	chirpSetProc("getFrame", (ProcPtr)getFrame);
	chirpSetProc("getRLSFrame", (ProcPtr)getRLSFrame);

#if 0
	while(1)
	{
		CTIMER_START();
		syncM1((uint32_t *)&LPC_GPIO_PORT->PIN[1], 0x2000);
		CTIMER_STOP();
		
		printf("%d\n", CTIMER_GET());
	}	
#endif
#if 0
{
	uint32_t i;
	uint8_t *lut = (uint8_t *)SRAM0_LOC + 0x10000;
	uint32_t memory = SRAM0_LOC;
	uint32_t size = SRAM0_SIZE/2;
	for (i=0; i<0x10000; i++)
		lut[i] = 0;
	lut[0xb400] = 0;
	lut[0xb401] = 1;
	lut[0xb402] = 1;
	lut[0xb403] = 1;
	lut[0xb404] = 0;
	lut[0xb405] = 1;
	lut[0xb406] = 1;
	lut[0xb407] = 0;
	lut[0xb408] = 0;
	lut[0xb409] = 0;

	while(1)
 		getRLSFrame(&memory, &size, (uint32_t *)&lut);
}
#endif
	printf("M0 ready\n");
	while(1)
		chirpService();
}
