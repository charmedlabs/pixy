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

#ifndef _RLS_M0_H
#define _RLS_M0_H

//#include <inttypes.h>
#include <stdint.h>

 #define __ASM            __asm                                      /*!< asm keyword for GNU Compiler          */
 #define __INLINE         inline                                     /*!< inline keyword for GNU Compiler       */
 #define __STATIC_INLINE  static inline

int rls_init(void);
int32_t getRLSFrame(uint32_t *m0Mem, uint32_t *lut);

#ifndef RLTEST
#define RED   	"LDRB   r6, [r0]"
#define GREEN	"LDRB   r5, [r0]"

#else // RLTEST
#define RED   asm("LDRB r6, [r0, r4]");
#define GREEN
		asm("LDRB r5, [r0, r4]");
		asm("ADDS r4, #1");
#endif	// RLTEST


#define EOL_CHECK \
asm( \
		"CMP r4, r9 \n\t" \
		"BGE eol"         \
)

// q val:
// | 4 bits    | 7 bits		 | 9 bits | 9 bits    | 3 bits |
// | shift val | shifted sum | length | begin col | model  |
//		MACRO // create q val
#define	QVAL {                  \
 	asm("MOV 	r5, r10");		\
	asm("LSLS 	r6, r5, #3");	\
	asm("ORRS	r6, r7");		\
	asm("SUBS  	r1, r4, r5");	\
	asm("LSLS 	r5, r1, #12");	\
	asm("ORRS 	r6, r5");		\
	asm("LDR	r5, [sp, #0x24]");	\
	asm("LDRB 	r1, [r1, r5]");	\
	asm("MOV 	r5, r8");		\
	asm("LSRS 	r5, r1");		\
	asm("LSLS	r5, #21");		\
	asm("ORRS 	r6, r5");		\
	asm("LSLS 	r1, #28");		\
	asm("ORRS	r6, r1");		\
	asm("MOV 	r1, r12");		\
	asm("MOVS 	r7, #4");		\
	asm(GREEN);					\
	asm("STR 	r6, [r1]");		\
	asm("ADD  	r12, r7");		\
	}

#endif
