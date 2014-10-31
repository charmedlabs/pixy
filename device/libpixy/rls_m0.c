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
#include "pixyvals.h"


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

#define NEWLINE
#ifndef NEWLINE

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

#else

__asm void lineProcessedRL0A(uint32_t *gpio, uint8_t *memory, uint32_t width) // width in bytes
{ 
// r0: gpio
// r1: memory
// r2: end mem
// r3: scratch
// r4: scratch
// r5: scratch
// r6: lastv
// r7: lastb+g
		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r4-r7, lr}

		LSLS	r2, #3 // scale ending by 8
		// add width to memory pointer so we can compare
		ADDS	r2, r1
		// generate hsync bit
	  	MOVS	r7, #0x1
		LSLS	r7, #11

		PUSH	{r0-r3} // save args
		BL.W	callSyncM1 // get pixel sync
		POP		{r0-r3}	// restore args
	   
	   	// pixel sync starts here

		// wait for hsync to go high
dest10B	LDR 	r3, [r0] 	// 2
		TST		r3, r7		// 1
		BEQ		dest10B		// 3

		// variable delay --- get correct phase for sampling
		NOP
		NOP

loop5B
		LDRB 	r3, [r0] // blue
		// cycle
		EORS	r6, r6
		EORS	r7, r7
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP

loop6B
		LDRB 	r4, [r0] // green 	 
		// cycle 
		ADDS    r5, r3, r4	// blue+green
		ADDS	r7, r5	// blue+green+lastb+g
		STRH 	r7, [r1, #2] // store b+gsum
		// cycle
		MOV		r7, r5 // save lastb+g
		SUBS	r4, r3, r4   // blue-green (v)
		ADDS	r6, r4
		LSLS	r6, #16
		STR	    r6,	[r1, #4] // store vsum
		// cycle 
		// pixel sync
 		LDRB 	r3, [r0] // blue
		// cycle
		MOV		r6, r4 // save lastv
		LSLS	r4, #23  // shift b-g and get rid of higher-order bits
		LSRS	r4, #26  // shift it back down and throw out the 3 lsbs
		STRH    r4,	[r1, #0] // store shifted v
		// cycle
		ADDS	r1, #8
		CMP		r1, r2
		BLT		loop6B

	   	// generate hsync bit
		MOVS	r7, #0x1
		LSLS	r7, #11
		// wait for hsync to go low (end of line)	
dest11B	LDR 	r3, [r0] 	// 2
		TST		r3, r7		// 1
		BNE		dest11B		// 3

		POP		{r4-r7, pc}
}

#endif

#ifndef NEWLINE 
  
__asm uint32_t lineProcessedRL1A(uint32_t *gpio, Qval *memory, uint8_t *lut, uint8_t *linestore, uint32_t width, uint8_t *shiftLut, 
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
		// which is enough time to copy 64 qvals (256 bytes), maximum qvals/line = 320/5
		// (this has been verified/tested)
		// The advantage of doing this is that we don't need to buffer much data
		// and it reduces the latency-- we can start processing qvals immediately
		// We need to copy these because the memory the qvals comes from must not be 
		// accessed by the M4, or wait states will be thrown in and we'll lose pixel sync for that line
		MOV		r0, r12  // qval pointer
		LDR		r1, [sp] // bring in original q memory location 
		SUBS	r0, r1 // get number of qvals*4

		LDR		r2, [sp, #0x28] // bring in qq memory pointer 
		LDR		r3, [sp, #0x2c] // bring in qq index
		LSLS 	r3, #2 // qq index in bytes (4 bytes/qval)
		LDR		r4, [sp, #0x30] // bring in qq size
		LSLS 	r4, #2 // qq size in bytes (4 bytes/qval)

		MOVS	r5, #0

lcpy	CMP		r0, r5	  // 1 end condition
		BEQ		ecpy	  // 1 exit

		LDR		r6, [r1, r5]  // 2 copy (read)
		STR		r6, [r2, r3]  // 2 copy (write)

		ADDS	r3, #4	  // 1 inc qq index
		ADDS	r5, #4	  // 1 inc counter

		CMP		r4, r3    // 1 check for qq index wrap
		BEQ		wrap	  // 1
		B		lcpy	  // 3

wrap	MOVS	r3, #0    // reset qq index
		B lcpy

ecpy	LSRS    r0, #2 // return number of qvals
		POP		{r1-r7, pc}
} 

#else

__asm uint32_t lineProcessedRL1A(uint32_t *gpio, Qval *memory, uint8_t *lut, uint8_t *linestore, uint32_t width, 
	Qval *qqMem, uint32_t qqIndex, uint32_t qqSize) // width in bytes
{
// r0: gpio	register
// r1: scratch 
// r2: lut
// r3: prev line
// r4: column 
// r5: scratch
// r6: scratch
// r7: prev model
// r8: red pixel sum (for ysum)
// r9: ending column
// r10: usum
// r11: ?
// r12:	q memory 
   
		MACRO // check for end of line
$lx		EOL_CHECK
$lx		CMP 	r4, r9
		BGE		eol
	   	MEND

		PRESERVE8
		IMPORT 	callSyncM1

		PUSH	{r1-r7, lr}
		// bring in ending column
		LDR		r4, [sp, #0x20]
		LSLS	r4, #3 // scale ending count by 8
	   	MOV		r9, r4  // move into r9
	  	MOVS	r5, #0x1
		LSLS	r5, #11	 // create hsync bit mask

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
		LDRB 	r5, [r0] // load green pixel 
		// cycle
		NOP
		NOP
		NOP
		SUBS	r4, #8
beg0	ADDS 	r4, #8 // inc col 
beg1	EOL_CHECK
		// cycle
		// *** PIXEL SYNC 
		LDRB 	r6, [r0] // load red pixel 
		// cycle
		SUBS	r5, r6, r5   // red-green
		MOV		r10, r5 // save red-green
		ASRS	r5, #3	 // reduce 9 to 6 bits arithmetically
		LSLS	r5, #26  // shift red-green and get rid of higher-order bits
		LSRS	r5, #20  // shift red-green back, make it the higher 6 bits of the index
		LDRH	r1, [r3, r4] // load shifted blue-green val
		// cycle
		ORRS	r1, r5   // form 12-bit index
		LDRB	r7, [r2, r1] // load lut val
		// cycle
		// *** PIXEL SYNC
		LDRB 	r5, [r0] // load green pixel 
		// cycle
		MOV		r8, r6 // save red
		NOP
		NOP
		CMP		r7, #0
		BEQ		beg0
		ADDS 	r4, #8 // inc col 
		EOL_CHECK
		// cycle
		NOP
		NOP
		// 2nd pixel
		// *** PIXEL SYNC
		LDRB 	r6, [r0] // load red pixel 
		// cycle
		SUBS	r5, r6, r5   // red-green
		ADD 	r10, r5	 // usum
		ASRS	r5, #3	 // reduce 9 to 6 bits arithmetically
		LSLS	r5, #26  // shift red-green and get rid of higher-order bits
		LSRS	r5, #20  // shift red-green back, make it the higher 8 bits of the index
		LDRB	r1, [r3, r4] // load shifted blue-green val
		// cycle
		ORRS	r1, r5   // form 12-bit index
		LDRB	r1, [r2, r1] // load lut val
		// cycle
		// *** PIXEL SYNC
		LDRB 	r5, [r0] // load green pixel 
		// cycle
		ADD		r8, r6   // add red
		NOP
		MOV		r6, r10	 // bring in usum
		CMP		r1, r7
		BNE		beg0  
		// ************ store qvals 
 		MOV		r5, r12	 // bring in qmem pointer
 		STRH	r6, [r5, #4] // store usum
		// cycle
		ORRS	r7, r4, r7  // combine signature and index
		ADDS	r4, #2	 // increment line mem
		// *** PIXEL SYNC RED
		LDRH	r1, [r3, r4] // load b+gsum
		// cycle 
		ADD		r1, r8   // add red sum to b+gsum to create ysum
		STRH	r1, [r5, #6] // store ysum
		// cycle
		ADDS	r4, #2	 // increment line mem
		LDR		r1, [r3, r4] // load vsum which is shifted by 16
		// cycle
		ORRS	r1, r7 // combine vsum with signature and index
		STR		r1, [r5] // store vsum
		// cycle
		NOP
		// *** PIXEL SYNC GREEN
		LDRB 	r5, [r0] // load green pixel 
		// cycle
		MOVS	r1, #8
		ADD		r12, r1	// inc qmem
		ADDS 	r4, #12  // inc col, skipped pixel
		NOP
		NOP
		B		beg1

	   	// wait for hsync to go low
eol		MOVS	r5, #0x1
		LSLS	r5, #11
dest20A	LDR 	r6, [r0] 	// 2
		TST		r6, r5		// 1
		BNE		dest20A		// 3

		MOV		r0, r12  // qval pointer
		LDR		r1, [sp] // bring in original q memory location 
		SUBS	r0, r1 // get number of qvals*6, return this number
		POP		{r1-r7, pc}

} 

#endif

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
#ifndef NEWLINE 
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

#else
#define MAX_NEW_QVALS_PER_LINE   (320/3)

	uint8_t *lut2 = (uint8_t *)*lut;
	volatile uint32_t line;
	uint8_t *qvalStore, *qval;
	uint32_t numQvals;
	uint8_t *lineStore;

	lineStore = (uint8_t *)(*lut - 320*8); //(uint8_t *)(*m0Mem) + MAX_NEW_QVALS_PER_LINE*8+16;
   	qvalStore =	(uint8_t *)*m0Mem;
	skipLines(0);
	qval = qvalStore;
	*(uint16_t *)qval = 0xffff;
	qval += 8; 
	for (line=0; line<CAM_RES2_HEIGHT && qval<(uint8_t *)(SRAM1_LOC+SRAM1_SIZE-320*8-0x1000-16); line++, qval+=numQvals)  // start totalQvals at 1 because of start of frame value
	{
		*(uint16_t *)qval = 0x0000;
		qval += 8; 
		lineProcessedRL0A((uint32_t *)&CAM_PORT, lineStore, CAM_RES2_WIDTH); 
		numQvals = lineProcessedRL1A((uint32_t *)&CAM_PORT, (Qval *)qval, lut2, lineStore, CAM_RES2_WIDTH, g_qqueue->data, g_qqueue->writeIndex, QQ_MEM_SIZE);
	}

	return qval-qvalStore;
#endif
}

int rls_init(void)
{
	chirpSetProc("getRLSFrame", (ProcPtr)getRLSFrame);
	return 0;
}

