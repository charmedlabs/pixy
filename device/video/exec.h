#ifndef _EXEC_H
#define _EXEC_H

#include "chirp.hpp"

#define EXEC_MAX_PROGS   7

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
int exec_addProg(Program *prog);

int exec_runM0(uint8_t prog);
int exec_stopM0();

uint32_t exec_running();
int32_t exec_stop();
int32_t exec_run();
int32_t exec_runprog(const uint8_t &progNum);
int32_t exec_list();
 
#endif
