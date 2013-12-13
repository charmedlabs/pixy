#ifndef _RLS_M0_H
#define _RLS_M0_H

#include <inttypes.h>

int rls_init(void);
int32_t getRLSFrame(uint32_t *m0Mem, uint32_t *lut);

#endif
