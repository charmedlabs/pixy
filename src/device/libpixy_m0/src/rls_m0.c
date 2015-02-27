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

#include "rls_m0.h"
#include "frame_m0.h"
#include "chirp.h"
#include "qqueue.h"


//#define RLTEST
#define MAX_QVALS_PER_LINE 	CAM_RES2_WIDTH/5	 // width/5 because that's the worst case with noise filtering

uint8_t *g_logLut = NULL;

#if 0 // this is the old method, might have use down the road....
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
		asm("NOP");
		asm("NOP");

#if 0
loop5
		LDRB 	r3, [r0] 	  
		STRB 	r3, [r1]
		asm("NOP");
		asm("NOP");
		asm("NOP");
		ADDS	r1, #0x01
		CMP		r1, r2
		BLT		loop5
#else
loop5
		LDRB 	r3, [r0] // blue
		LSRS    r3, #3
		LSLS    r3, #10
		asm("NOP");
		asm("NOP");
		asm("NOP");
		asm("NOP");
		asm("NOP");
		asm("NOP");
		asm("NOP");
		asm("NOP");

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
	    asm("NOP");
		//(borrow asm("NOP"); below)
		// skip green pixel
		MOVS    r5, #0x00 // clear r5 (which will be copied to r7) This will force a write of a q val 
		MOVS	r4, #0x00 // clear col (col is numbered 1 to 320)
// 2
loop6	MOV		r7, r5 // copy lut val (lutPrev)
		MOV		r5, r8
		CMP		r3, r5
		BGE		dest30
		asm("NOP");
		asm("NOP");
		asm("NOP");
		asm("NOP");
		asm("NOP");
// 9
loop7	
		LDRH	r5, [r3] // load blue green val
// 2
		LDRB 	r6, [r0] // load red pixel
		LSRS    r6, #3	 // shift into place (5 bits of red)
		ORRS	r5, r6 // form 15-bit lut index
		ADDS    r3, #0x02 // inc prev line pointer
		ADDS	r4, #0x01 // inc col
		asm("NOP");
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

void lineProcessedRL0A(uint32_t *gpio, uint8_t *memory, uint32_t width) // width in bytes
{ 
//		PRESERVE8

	asm(".syntax unified");

		asm("PUSH	{r4-r5}");

		// add width to memory pointer so we can compare
		asm("ADDS	r2, r1");
		// generate hsync bit
		asm("MOVS	r5, #0x1");
		asm("LSLS	r5, #11");

		asm("PUSH	{r0-r3}"); // save args
		asm("BL.W	callSyncM1"); // get pixel sync
		asm("POP	{r0-r3}");	// restore args
	   
	   	// pixel sync starts here

		// wait for hsync to go high
asm("dest10A:");
		asm("LDR 	r3, [r0]"); 	// 2
		asm("TST	r3, r5");		// 1
		asm("BEQ	dest10A");		// 3

		// variable delay --- get correct phase for sampling
		asm("NOP");
		asm("NOP");

asm("loop5A:");
		asm("LDRB 	r3, [r0]"); // blue
		// cycle
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

		asm("LDRB 	r4, [r0]"); // green
		// cycle 
		asm("SUBS	r3, r4");   // blue-green
		asm("ASRS   r3, #1");
		asm("STRB   r3, [r1]"); // store blue-green
		// cycle 
		asm("ADDS   r1, #0x01");
		asm("CMP	r1, r2");
		asm("NOP");
		asm("BLT	loop5A");

		// wait for hsync to go low (end of line)
asm("dest11A:");
		asm("LDR 	r3, [r0]"); 	// 2
		asm("TST	r3, r5");		// 1
		asm("BNE	dest11A");		// 3

		asm("POP	{r4-r5}");

		asm(".syntax divided");
}


uint32_t lineProcessedRL1A(uint32_t *gpio, Qval *memory, uint8_t *lut, uint8_t *linestore, uint32_t width, uint8_t *shiftLut,
	Qval *qqMem, uint32_t qqIndex, uint32_t qqSize) // width in bytes
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

//		PRESERVE8

asm(".syntax unified");
	asm("PUSH	{r1-r7}");
	// bring in ending column
	asm("LDR	r4, [sp, #0x20]");
	asm("MOV	r9, r4");
	asm("MOVS	r5, #0x1");
	asm("LSLS	r5, #11");

	asm("PUSH	{r0-r3}"); // save args
	asm("BL.W	callSyncM1"); // get pixel sync
	asm("POP	{r0-r3}");	// restore args

	// pixel sync starts here

	// wait for hsync to go high
asm("dest12A:");
	asm("LDR 	r6, [r0]"); 	// 2
	asm("TST	r6, r5");		// 1
	asm("BEQ	dest12A");	// 3

	// variable delay --- get correct phase for sampling
	asm("MOV	r12, r1"); // save q memory
	asm("MOVS	r4, #0"); // clear column value

	// *** PIXEL SYNC (start reading pixels)
	asm(GREEN);
	// cycle
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");
	asm("NOP");

asm("zero0:");
	asm("MOVS	r6, #0");
	asm("MOV	r8, r6");	// clear sum (so we don't think we have an outstanding segment)
	EOL_CHECK;
	// cycle
	// *** PIXEL SYNC (check for nonzero lut value)
asm("zero1:");
	//LEXT r7
//	MACRO // create index, lookup, inc col, extract model
//	$lx		LEXT	$rx
	asm(RED);
	// cycle
	asm("SUBS	r6, r5");   // red-green
	asm("ASRS	r6, #1");	 // reduce 9 to 8 bits arithmetically
	asm("LSLS	r6, #24");  // shift red-green and get rid of higher-order bits
	asm("LSRS	r6, #16");  // shift red-green back, make it the higher 8 bits of the index
	asm("LDRB	r5, [r3, r4]"); // load blue-green val
	// cycle
	asm("ORRS	r5, r6");   // form 16-bit index
	asm("LDRB	r1, [r2, r5]"); // load lut val
	// cycle
	asm("ADDS 	r4, #1"); // inc col
	// *** PIXEL SYNC
	asm(GREEN);
	// cycle
	asm("LSLS	r7, r1, #29"); // knock off msb's
	asm("LSRS	r7, #29"); // extract model, put in rx
	//MEND

	// cycle
	// cycle
	// cycle
	asm("CMP 	r7, #0");
	asm("BEQ 	zero0");
	asm("MOV	r10, r4"); // save start column
	asm("ADD	r8, r1"); // add to sum
	EOL_CHECK;
	// cycle
	asm("NOP");
	asm("NOP");
	// *** PIXEL SYNC (check nonzero value for consistency)
//	LEXT r6;
	asm(RED);
	asm("SUBS	r6, r5");   // red-green
	asm("ASRS	r6, #1");	 // reduce 9 to 8 bits arithmetically
	asm("LSLS	r6, #24");  // shift red-green and get rid of higher-order bits
	asm("LSRS	r6, #16");  // shift red-green back, make it the higher 8 bits of the index
	asm("LDRB	r5, [r3, r4]"); // load blue-green val
	asm("ORRS	r5, r6");   // form 16-bit index
	asm("LDRB	r1, [r2, r5]"); // load lut val
	asm("ADDS 	r4, #1"); // inc col
	// *** PIXEL SYNC
	asm(GREEN);
	// cycle
	asm("LSLS	r6, r1, #29"); // knock off msb's
	asm("LSRS	r6, #29"); // extract model, put in rx
// end of LEXT r6

	// cycle
	// cycle
	// cycle
	asm("CMP	r6, r7");
	asm("BNE	zero0");
	asm("NOP");
	asm("NOP");
asm("one:");
	asm("MOV	r11, r1");	// save last lut val
	asm("ADD	r8, r1"); // add to sum
	EOL_CHECK;
	// cycle
	// *** PIXEL SYNC (run-length segment)

//	LEXT r6
	asm(RED);
	// cycle
	asm("SUBS	r6, r5");   // red-green
	asm("ASRS	r6, #1");	 // reduce 9 to 8 bits arithmetically
	asm("LSLS	r6, #24");  // shift red-green and get rid of higher-order bits
	asm("LSRS	r6, #16");  // shift red-green back, make it the higher 8 bits of the index
	asm("LDRB	r5, [r3, r4]"); // load blue-green val
	// cycle
	asm("ORRS	r5, r6");   // form 16-bit index
	asm("LDRB	r1, [r2, r5]"); // load lut val
	// cycle
	asm("ADDS 	r4, #1"); // inc col
	// *** PIXEL SYNC
	asm(GREEN);
	// cycle
	asm("LSLS	r6, r1, #29"); // knock off msb's
	asm("LSRS	r6, #29"); // extract model, put in rx
// end of LEXT

	// cycle
	// cycle
	// cycle
	asm("CMP	r6, r7");
	asm("BEQ	one");
	asm("ADD	r8, r11"); // need to add something-- use last lut val
	EOL_CHECK;
	// cycle
	asm("NOP");
	asm("NOP");
	asm("NOP");
	// *** PIXEL SYNC (1st pixel not equal)

//	LEXT r6
	asm(RED);
	// cycle
	asm("SUBS	r6, r5");   // red-green
	asm("ASRS	r6, #1");	 // reduce 9 to 8 bits arithmetically
	asm("LSLS	r6, #24");  // shift red-green and get rid of higher-order bits
	asm("LSRS	r6, #16");  // shift red-green back, make it the higher 8 bits of the index
	asm("LDRB	r5, [r3, r4]"); // load blue-green val
	// cycle
	asm("ORRS	r5, r6");   // form 16-bit index
	asm("LDRB	r1, [r2, r5]"); // load lut val
	// cycle
	asm("ADDS 	r4, #1"); // inc col
	// *** PIXEL SYNC
	asm(GREEN);
	// cycle
	asm("LSLS	r6, r1, #29"); // knock off msb's
	asm("LSRS	r6, #29"); // extract model, put in rx
// end of LEXT

	// cycle
	// cycle
	// cycle
	asm("CMP	r6, r7");
	asm("BEQ	one");
	// 2nd pixel not equal--- run length is done
	QVAL;
	asm("MOVS	r6, #0");
	asm("MOV	r8, r6"); // clear sum
	asm("ADDS	r4, #1"); // add column
	asm("NOP");
	asm("B 		zero1");

asm("eol:");
	// check r8 for unfinished q val
	asm("MOVS	r6, #0");
	asm("CMP	r8, r6");
	asm("BEQ	eol0");
	QVAL;

	// wait for hsync to go low
asm("eol0:");
	asm("MOVS	r5, #0x1");
	asm("LSLS	r5, #11");

asm("dest20A:");
	asm("LDR 	r6, [r0]"); 	// 2
	asm("TST	r6, r5");		// 1
	asm("BNE	dest20A");	// 3

// we have approx 1800 cycles to do something here
// which is enough time to copy 64 qvals (256 bytes), maximum qvals/line = 320/5
// (this has been verified/tested)
// The advantage of doing this is that we don't need to buffer much data
// and it reduces the latency-- we can start processing qvals immediately
// We need to copy these because the memory the qvals comes from must not be
// accessed by the M4, or wait states will be thrown in and we'll lose pixel sync for that line
	asm("MOV	r0, r12");  // qval pointer
	asm("LDR	r1, [sp]"); // bring in original q memory location
	asm("SUBS	r0, r1"); // get number of qvals*4

	asm("LDR	r2, [sp, #0x28]"); // bring in qq memory pointer
	asm("LDR	r3, [sp, #0x2c]"); // bring in qq index
	asm("LSLS 	r3, #2"); // qq index in bytes (4 bytes/qval)
	asm("LDR	r4, [sp, #0x30]");; // bring in qq size
	asm("LSLS 	r4, #2"); // qq size in bytes (4 bytes/qval)

	asm("MOVS	r5, #0");

asm("lcpy:");
	asm("CMP	r0, r5");	  // 1 end condition
	asm("BEQ	ecpy");	  // 1 exit

	asm("LDR	r6, [r1, r5]");  // 2 copy (read)
	asm("STR	r6, [r2, r3]");  // 2 copy (write)

	asm("ADDS	r3, #4");	  // 1 inc qq index
	asm("ADDS	r5, #4");	  // 1 inc counter

	asm("CMP	r4, r3");    // 1 check for qq index wrap
	asm("BEQ	wrap");	  // 1
	asm("B		lcpy");	  // 3

asm("wrap:");
	asm("MOVS	r3, #0");    // reset qq index
	asm("B 		lcpy");

asm("ecpy:");
	asm("LSRS   r0, #2"); // return number of qvals
	asm("POP	{r1-r7}");

	asm(".syntax divided");

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


int32_t getRLSFrame(uint32_t *m0Mem, uint32_t *lut)
{
	uint8_t *lut2 = (uint8_t *)*lut;
	Qval *qvalStore = (Qval *)*m0Mem;
	uint32_t line;
	uint32_t numQvals;
	uint32_t totalQvals;
	uint8_t *lineStore;
	uint8_t *logLut;

	lineStore = (uint8_t *)(qvalStore + MAX_QVALS_PER_LINE);
	logLut = lineStore + CAM_RES2_WIDTH + 4;
	// m0mem needs to be at least 64*4 + CAM_RES2_WIDTH*2 + 4 =	900 ~ 1024

	if (g_logLut!=logLut)
	{
		g_logLut = logLut; 
	 	createLogLut();
	}

	// don't even attempt to grab lines if we're lacking space...
	if (qq_free()<MAX_QVALS_PER_LINE)
		return -1; 

	// indicate start of frame
	qq_enqueue(0xffffffff); 
	skipLines(0);
	for (line=0, totalQvals=1; line<CAM_RES2_HEIGHT; line++)  // start totalQvals at 1 because of start of frame value
	{
		// not enough space--- return error
		if (qq_free()<MAX_QVALS_PER_LINE)
			return -1; 
		// mark beginning of this row (column 0 = 0)
		// column 1 is the first real column of pixels
		qq_enqueue(0); 
		lineProcessedRL0A((uint32_t *)&CAM_PORT, lineStore, CAM_RES2_WIDTH); 
		numQvals = lineProcessedRL1A((uint32_t *)&CAM_PORT, qvalStore, lut2, lineStore, CAM_RES2_WIDTH, g_logLut, g_qqueue->data, g_qqueue->writeIndex, QQ_MEM_SIZE);
		// modify qq to reflect added data
		g_qqueue->writeIndex += numQvals;
		if (g_qqueue->writeIndex>=QQ_MEM_SIZE)
			g_qqueue->writeIndex -= QQ_MEM_SIZE;
		g_qqueue->produced += numQvals;
		totalQvals += numQvals+1; // +1 because of beginning of line 
	}
	return 0;
}


int rls_init(void)
{
	chirpSetProc("getRLSFrame", (ProcPtr)getRLSFrame);
	return 0;
}

