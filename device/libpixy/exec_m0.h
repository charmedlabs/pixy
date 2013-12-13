#ifndef _EXEC_M0_H
#define _EXEC_M0_H

#include <inttypes.h>

int exec_init(void);
uint32_t exec_running(void);
int32_t exec_stop(void);
int32_t exec_run(uint8_t *prog);
void exec_loop(void);

#endif
