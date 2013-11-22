#ifndef _EXEC_H
#define _EXEC_H

void exec_loop();
int exec_init(Chirp *chirp);
uint32_t exec_running();
int32_t exec_stop();
int32_t exec_run(const char *prog);
int32_t exec_list();

#endif
