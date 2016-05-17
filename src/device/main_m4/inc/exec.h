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

#ifndef _EXEC_H
#define _EXEC_H

#include "chirp.hpp"
#include "debug_frmwrk.h"

//#define LEGO

#define FW_MAJOR_VER		2
#define FW_MINOR_VER		0
#define FW_BUILD_VER		19
#ifdef LEGO
#define FW_TYPE             "LEGO"
#else
#define FW_TYPE             "general"
#endif

#define EXEC_MAX_PROGS   9
#define EXEC_VIDEO_PROG  EXEC_MAX_PROGS-1

typedef int (*ProgFunc)();

struct Program
{  	
	char *progName;
	char *desc;
	ProgFunc setup;
	ProgFunc loop;
};

struct ActionScriptlet
{
	const char *action;
	const char *scriptlet;
};

void exec_loop();
int exec_init(Chirp *chirp);
void exec_select();
int exec_addProg(Program *prog, bool video=false);

int exec_runM0(uint8_t prog);
int exec_stopM0();
void exec_periodic();

uint32_t exec_running();
int32_t exec_stop();
int32_t exec_run();
int32_t exec_runprog(const uint8_t &progNum, Chirp *chirp=NULL);
int32_t exec_runprogArg(const uint8_t &progNum, const int32_t &arg, Chirp *chirp=NULL);
int32_t exec_list();
int32_t exec_version(Chirp *chirp=NULL);
int32_t exec_versionType(Chirp *chirp=NULL);

int32_t exec_getAction(const uint16_t &index, Chirp *chirp=NULL);
uint32_t exec_getUID();

void exec_loadParams();
void exec_sendEvent(Chirp *chirp, uint32_t event);

uint8_t exec_pause();
void exec_resume();

extern int32_t g_execArg; 

#endif
