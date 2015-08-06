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
#include "assembly.h"

//#define RLTEST
#define MAX_NEW_QVALS_PER_LINE   ((CAM_RES2_WIDTH/3)+2)

_ASM_FUNC void lineProcessedRL0A(uint32_t *gpio, uint8_t *memory, uint32_t width) // width in bytes
{ 
// r0: gpio
// r1: memory
// r2: end mem
// r3: scratch
// r4: scratch
// r5: scratch
// r6: lastv
// r7: lastb+g
_ASM_START
	_ASM_IMPORT(callSyncM1)

#ifdef KEIL
	_ASM(PUSH	{r4-r7, lr})
#else
	_ASM(PUSH	{r4-r7})
#endif

	_ASM(LSLS	r2, #3) // scale ending by 8
	// add width to memory pointer so we can compare
	_ASM(ADDS	r2, r1)
	// generate hsync bit
  	_ASM(MOVS	r7, #0x1)
	_ASM(LSLS	r7, #11)

	_ASM(PUSH	{r0-r3}) // save args
	_ASM(BL.W	callSyncM1) // get pixel sync
	_ASM(POP	{r0-r3})	// restore args
	   
   	// pixel sync starts here

	// wait for hsync to go high
_ASM_LABEL(dest10B)	
	_ASM(LDR 	r3, [r0]) 	// 2
	_ASM(TST	r3, r7)		// 1
	_ASM(BEQ	dest10B)		// 3

	// variable delay --- get correct phase for sampling
	_ASM(NOP)
	_ASM(NOP)

_ASM_LABEL(loop5B)
	_ASM(LDRB 	r3, [r0]) // blue
	// cycle
	_ASM(EORS	r6, r6)
	_ASM(EORS	r7, r7)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)

_ASM_LABEL(loop6B)
	_ASM(LDRB 	r4, [r0]) // green 	 
	// cycle 
	_ASM(ADDS   r5, r3, r4)	// blue+green
	_ASM(ADDS	r7, r5)	// blue+green+lastb+g
	_ASM(STRH 	r7, [r1, #2]) // store b+gsum
	// cycle
	_ASM(MOV	r7, r5) // save lastb+g
	_ASM(SUBS	r4, r3, r4)   // blue-green (v)
	_ASM(ADDS	r6, r4)
	_ASM(LSLS	r6, #16)
	_ASM(STR	r6,	[r1, #4]) // store vsum
	// cycle 
	// pixel sync
 	_ASM(LDRB 	r3, [r0]) // blue
	// cycle
	_ASM(MOV	r6, r4) // save lastv
	_ASM(LSLS	r4, #23)  // shift b-g and get rid of higher-order bits
	_ASM(LSRS	r4, #26)  // shift it back down and throw out the 3 lsbs
	_ASM(STRH   r4,	[r1, #0]) // store shifted v
	// cycle
	_ASM(ADDS	r1, #8)
	_ASM(CMP	r1, r2)
	_ASM(BLT	loop6B)

	// generate hsync bit
	_ASM(MOVS	r7, #0x1)
	_ASM(LSLS	r7, #11)
	// wait for hsync to go low (end of line)	
_ASM_LABEL(dest11B)
	_ASM(LDR 	r3, [r0]) 	// 2
	_ASM(TST	r3, r7)		// 1
	_ASM(BNE	dest11B)		// 3

#ifdef KEIL
	_ASM(POP	{r4-r7, pc})
#else
	_ASM(POP	{r4-r7})
#endif

	_ASM_END
}

_ASM_FUNC uint32_t lineProcessedRL1A(uint32_t *gpio, Qval *memory, uint8_t *lut, uint8_t *linestore, uint32_t width, 
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
   
	_ASM_START
	_ASM_IMPORT(callSyncM1)

#ifdef KEIL
	_ASM(PUSH	{r1-r7, lr})
#else
	_ASM(PUSH	{r1-r7})
#endif

	// bring in ending column
#ifdef KEIL
	_ASM(LDR	r4, [sp, #0x20])
#else
	_ASM(LDR	r4, [sp, #0x34])
#endif
	_ASM(LSLS	r4, #3) // scale ending count by 8
	_ASM(MOV	r9, r4)  // move into r9
	_ASM(MOVS	r5, #0x1)
	_ASM(LSLS	r5, #11)	 // create hsync bit mask

	_ASM(PUSH	{r0-r3}) // save args
	_ASM(BL.W	callSyncM1) // get pixel sync
	_ASM(POP	{r0-r3})	// restore args

	// pixel sync starts here
		
	// wait for hsync to go high
_ASM_LABEL(dest12A)
	_ASM(LDR 	r6, [r0]) 	// 2
	_ASM(TST	r6, r5)		// 1
	_ASM(BEQ	dest12A)		// 3

	// variable delay --- get correct phase for sampling
	_ASM(MOV	r12, r1) // save q memory
	_ASM(MOVS	r4, #0) // clear column value

	// *** PIXEL SYNC (start reading pixels)
	_ASM(LDRB 	r5, [r0]) // load green pixel 
	// cycle
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(SUBS	r4, #8)
_ASM_LABEL(beg0)
	_ASM(ADDS 	r4, #8) // inc col 
_ASM_LABEL(beg1)
	// EOL CHECK
	_ASM(CMP	r4, r9)
	_ASM(BGE	eol)
	// cycle
	// *** PIXEL SYNC 
	_ASM(LDRB 	r6, [r0]) // load red pixel 
	// cycle
	_ASM(SUBS	r5, r6, r5)   // red-green
	_ASM(MOV	r10, r5) // save red-green
	_ASM(ASRS	r5, #3)	 // reduce 9 to 6 bits arithmetically
	_ASM(LSLS	r5, #26)  // shift red-green and get rid of higher-order bits
	_ASM(LSRS	r5, #20)  // shift red-green back, make it the higher 6 bits of the index
	_ASM(LDRH	r1, [r3, r4]) // load shifted blue-green val
	// cycle
	_ASM(ORRS	r1, r5)   // form 12-bit index
	_ASM(LDRB	r7, [r2, r1]) // load lut val
	// cycle
	// *** PIXEL SYNC
	_ASM(LDRB 	r5, [r0]) // load green pixel 
	// cycle
	_ASM(MOV	r8, r6) // save red
	_ASM(NOP)
	_ASM(NOP)
	_ASM(CMP	r7, #0)
	_ASM(BEQ	beg0)
	_ASM(ADDS 	r4, #8) // inc col 
	// EOL CHECK
	_ASM(CMP	r4, r9)
	_ASM(BGE	eol)
	// cycle
	_ASM(NOP)
	_ASM(NOP)
	// 2nd pixel
	// *** PIXEL SYNC
	_ASM(LDRB 	r6, [r0]) // load red pixel 
	// cycle
	_ASM(SUBS	r5, r6, r5)   // red-green
	_ASM(ADD 	r10, r5)	 // usum
	_ASM(ASRS	r5, #3)	 // reduce 9 to 6 bits arithmetically
	_ASM(LSLS	r5, #26)  // shift red-green and get rid of higher-order bits
	_ASM(LSRS	r5, #20)  // shift red-green back, make it the higher 8 bits of the index
	_ASM(LDRB	r1, [r3, r4]) // load shifted blue-green val
	// cycle
	_ASM(ORRS	r1, r5)   // form 12-bit index
	_ASM(LDRB	r1, [r2, r1]) // load lut val
	// cycle
	// *** PIXEL SYNC
	_ASM(LDRB 	r5, [r0]) // load green pixel 
	// cycle
	_ASM(ADD	r8, r6)   // add red
	_ASM(NOP)
	_ASM(MOV	r6, r10)	 // bring in usum
	_ASM(CMP	r1, r7)
	_ASM(BNE	beg0)  
	// ************ store qvals 
 	_ASM(MOV	r5, r12)	 // bring in qmem pointer
 	_ASM(STRH	r6, [r5, #4]) // store usum
	// cycle
	_ASM(ORRS	r7, r4, r7)  // combine signature and index
	_ASM(ADDS	r4, #2)	 // increment line mem
	// *** PIXEL SYNC RED
	_ASM(LDRH	r1, [r3, r4]) // load b+gsum
	// cycle 
	_ASM(ADD	r1, r8)   // add red sum to b+gsum to create ysum
	_ASM(STRH	r1, [r5, #6]) // store ysum
	// cycle
	_ASM(ADDS	r4, #2)	 // increment line mem
	_ASM(LDR	r1, [r3, r4]) // load vsum which is shifted by 16
	// cycle
	_ASM(ORRS	r1, r7) // combine vsum with signature and index
	_ASM(STR	r1, [r5]) // store vsum
	// cycle
	_ASM(NOP)
	// *** PIXEL SYNC GREEN
	_ASM(LDRB 	r5, [r0]) // load green pixel 
	// cycle
	_ASM(MOVS	r1, #8)
	_ASM(ADD	r12, r1)	// inc qmem
	_ASM(ADDS 	r4, #12)  // inc col, skipped pixel
	_ASM(NOP)
	_ASM(NOP)

#if 1
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
	_ASM(NOP)
	_ASM(NOP)
	_ASM(NOP)
	_ASM(ADDS 	r4, #8)  // inc col, skipped pixel
#endif

	_ASM(B		beg1)

	// wait for hsync to go low
_ASM_LABEL(eol)	
	_ASM(MOVS	r5, #0x1)
	_ASM(LSLS	r5, #11)
_ASM_LABEL(dest20A)
	_ASM(LDR 	r6, [r0]) 	// 2
	_ASM(TST	r6, r5)		// 1
	_ASM(BNE	dest20A)	// 3

	// we have approx 1800 cycles to do something here
	// The advantage of doing this is that we don't need to buffer much data
	// and it reduces the latency-- we can start processing qvals immediately
	// We need to copy these because the memory the qvals comes from must not be 
	// accessed by the M4, or wait states will be thrown in and we'll lose pixel sync for that line
#ifdef KEIL
	_ASM(LDR	r1, [sp]) // bring in original q memory location 
	_ASM(LDR	r2, [sp, #0x24]) // bring in qq memory pointer 
	_ASM(LDR	r3, [sp, #0x28]) // bring in qq index
#else
	_ASM(LDR	r1, [sp]) // bring in original q memory location
	_ASM(LDR	r2, [sp, #0x38]) // bring in qq memory pointer
	_ASM(LDR	r3, [sp, #0x3c]) // bring in qq index
#endif
	_ASM(LSLS 	r3, #3) // qq index in bytes (4 bytes/qval)
	_ASM(ADDS	r3, r2)
#ifdef KEIL
	_ASM(LDR	r4, [sp, #0x2c]) // bring in qq size
#else
	_ASM(LDR	r4, [sp, #0x40]) // bring in qq size
#endif
	_ASM(LSLS 	r4, #3) // qq size in bytes (4 bytes/qval)
	_ASM(ADDS	r4, r2)

_ASM_LABEL(lcpy)
	_ASM(CMP	r1, r12)	  // 1 end condition
	_ASM(BEQ	ecpy)	  // 1 exit

	_ASM(LDR	r0, [r1, #0])  // 2 copy (read)
	_ASM(STR	r0, [r3, #0])  // 2 copy (write)
	_ASM(LDR	r0, [r1, #4])  // 2 copy (read)
	_ASM(STR	r0, [r3, #4])  // 2 copy (write)

	_ASM(ADDS	r1, #8)	  // 1 inc qval index
	_ASM(ADDS	r3, #8)	  // 1 inc qq counter

	_ASM(CMP	r4, r3)    // 1 check for qq index wrap
	_ASM(BEQ	wrap)	  // 1
	_ASM(B		lcpy)	  // 3

_ASM_LABEL(wrap)
#ifdef KEIL
	_ASM(LDR	r3, [sp, #0x24]) // bring in qq memory pointer 
#else
	_ASM(LDR	r3, [sp, #0x38]) // bring in qq memory pointer
#endif
	_ASM(B 		lcpy)


_ASM_LABEL(ecpy)
	_ASM(MOV	r0, r12)  // qval pointer
	_ASM(LDR	r1, [sp]) // bring in original q memory location 
	_ASM(SUBS	r0, r1) // get number of qvals*8, return this number
	_ASM(LSRS	r0, #3) // divide by 8, this is the return value
#ifdef KEIL
	_ASM(POP	{r1-r7, pc})
#else
	_ASM(POP	{r1-r7})
	_ASM(MOVS	r3, r0) // gcc is using r3 to return values?
#endif
	_ASM_END
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

int g_foo = 0;

int32_t getRLSFrame(uint32_t *m0Mem, uint32_t *lut)
{
	uint8_t *lut2 = (uint8_t *)*lut;
	uint32_t line;
	Qval *qvalStore;
	uint32_t numQvals;
	uint8_t *lineStore;
	Qval lineBegin, frameEnd;
	lineBegin.m_col = lineBegin.m_u = lineBegin.m_v = lineBegin.m_y = 0;
	frameEnd.m_col = 0xffff;
	frameEnd.m_u = frameEnd.m_v = frameEnd.m_y = 0;

//	if (!g_foo)
//		return 0;
   	qvalStore =	(Qval *)*m0Mem;
	lineStore = (uint8_t *)*m0Mem + MAX_NEW_QVALS_PER_LINE*sizeof(Qval);
	skipLines(0);
	for (line=0; line<CAM_RES2_HEIGHT; line++) 
	{
		// not enough space--- return error
		if (qq_free()<MAX_NEW_QVALS_PER_LINE)
		{
			frameEnd.m_col = 0xfffe;
			qq_enqueue(&frameEnd);
			//printf("*\n");
			return -1;
		} 
		qq_enqueue(&lineBegin); 
		lineProcessedRL0A((uint32_t *)&CAM_PORT, lineStore, CAM_RES2_WIDTH); 
		numQvals = lineProcessedRL1A((uint32_t *)&CAM_PORT, qvalStore, lut2, lineStore, CAM_RES2_WIDTH, g_qqueue->data, g_qqueue->writeIndex, QQ_MEM_SIZE);
		g_qqueue->writeIndex += numQvals;
		if (g_qqueue->writeIndex>=QQ_MEM_SIZE)
			g_qqueue->writeIndex -= QQ_MEM_SIZE;
		g_qqueue->produced += numQvals;
	}
	qq_enqueue(&frameEnd);

	return 0;
}

int rls_init(void)
{
	chirpSetProc("getRLSFrame", (ProcPtr)getRLSFrame);
	return 0;
}

