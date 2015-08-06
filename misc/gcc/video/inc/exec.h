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

#define FW_MAJOR_VER		0
#define FW_MINOR_VER		1
#define FW_BUILD_VER		44

#define EXEC_MAX_PROGS   8
#define EXEC_VIDEO_PROG  EXEC_MAX_PROGS

typedef int (*ProgFunc)();

struct Program
{  	
	char *progName;
	char *desc;
	ProgFunc setup;
	ProgFunc loop;
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
int32_t exec_runprog(const uint8_t &progNum);
int32_t exec_runprogArg(const uint8_t &progNum, const int32_t &arg);
int32_t exec_list();
int32_t exec_version(Chirp *chirp=NULL);

void exec_loadParams();

extern int32_t g_execArg; 
#endif
