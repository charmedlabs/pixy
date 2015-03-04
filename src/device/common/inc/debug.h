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

#ifndef DEBUG_H
#define DEBUG_H

#ifdef CORE_M0
#include <stdio.h>
#else
#ifdef __cplusplus  
#include "chirp.hpp"
#endif
#endif
#include "debug_frmwrk.h"

#ifdef CORE_M0
#define printf(...)  printf(__VA_ARGS__)
#else
#define printf(...)  lpc_printf(__VA_ARGS__)

#ifdef __cplusplus  
extern uint8_t g_debug;

#define DBG(...)            if (g_debug) cprintf(__VA_ARGS__)
#define DBGL(level, ...)    if (g_debug>=level) cprintf(__VA_ARGS__)
#define DBGE(n, ...)        if (g_debug==n) cprintf(__VA_ARGS__)
#endif
#endif

#endif
