#ifndef _ASSEMBLY_H
#define _ASSEMBLY_H

#ifdef KEIL

#define _ASM(...)            __VA_ARGS__
#define _ASM_FUNC            __asm
#define _ASM_LABEL(label)    label
#define _ASM_START\
	PRESERVE8
#define _ASM_END
#define _ASM_IMPORT(func)\
	IMPORT func

#else

// PP_NARG thanks to Laurent Deniau
#define PP_NARG(...)         PP_NARG_(__VA_ARGS__,PP_RSEQ_N())
#define PP_NARG_(...)        PP_ARG_N(__VA_ARGS__)
#define PP_ARG_N(_1, _2, _3, _4, _5,N,...) N
#define PP_RSEQ_N()          5,4,3,2,1,0
		
// APPLYXn variadic X-Macro by M Joshua Ryan   
#define X(x)                 #x
#define Paste(a,b)           a ## b
#define XPASTE(a,b)          Paste(a,b)
#define APPLYX1(a)           X(a)
#define APPLYX2(a,b)         X(a) ", " X(b)
#define APPLYX3(a,b,c)       X(a) ", " X(b) ", " X(c)
#define APPLYX4(a,b,c,d)     X(a) ", " X(b) ", " X(c) ", " X(d)
#define APPLYX5(a,b,c,d,e)   X(a) ", " X(b) ", " X(c) ", " X(d) ", " X(e)
#define APPLYX_(M, ...)      M(__VA_ARGS__)
#define APPLYXn(...)         APPLYX_(XPASTE(APPLYX, PP_NARG(__VA_ARGS__)), __VA_ARGS__)

#define _ASM(...)            asm(APPLYXn(__VA_ARGS__));
#define _ASM_FUNC
#define _ASM_LABEL(label)    asm(#label ":");
#define _ASM_START           asm(".syntax unified"); 
#define _ASM_END             asm(".syntax divided");
#define _ASM_IMPORT(func)    

#endif
#endif
