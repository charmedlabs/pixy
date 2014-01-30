#ifndef DEBUG_H
#define DEBUG_H

#ifdef CORE_M0
#include <stdio.h>
#endif
#include "debug_frmwrk.h"

#ifdef CORE_M0
#define printf(...)  printf(__VA_ARGS__)
#else
#define printf(...)  lpc_printf(__VA_ARGS__)
#endif

#endif
