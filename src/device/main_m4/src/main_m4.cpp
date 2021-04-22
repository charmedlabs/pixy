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

#include <string.h>
#include <stdio.h>
#include <debug.h>
#include <pixy_init.h>
#include <pixyvals.h>
#include <pixy_init.h>
#include <misc.h>
#include "camera.h"
#include "led.h"
#include "exec.h"
#include "camera.h"
#include "progvideo.h"
#include "progblobs.h"
#include "serial.h"

#include <new>

// M0 code
#ifdef KEIL
const // so m0 program goes into RO memory
#include "../main_m0/m0_image.c"
#endif

extern "C"
{
// For some strange reason, putting this routine in libpixy messes with the debugger
// or the execution--- not sure which.
// this is called if we allocate memory (new) and don't catch exception
// it may be called for other reasons too...
void __default_signal_handler(int signal, int type)
{
    char message[48];

    sprintf(message, "received signal: %d %d\n", signal, type);
    showError(signal, 0xff0000, message);
}

#if 0
unsigned __check_heap_overflow (void * new_end_of_heap)
{

    if ((uint32_t)new_end_of_heap >= 0x20008000-0x600)
        return 1; // Heap has overflowed
    return 0; // Heap still OK
}
#endif
}


int main(void)
{
    uint16_t major, minor, build;
    char *type;
    int i, res, count, count2;
    volatile uint32_t d;

    // insert a small delay so power supply can stabilize
    for (d=0; d<2500000; d++);

#ifdef KEIL
    pixyInit(SRAM3_LOC, &LR0[0], sizeof(LR0));
#else
    pixyInit();
#endif

    // main init of hardware plus a version-dependent number for the parameters that will
    // force a format of parameter between version numbers.
    exec_init(g_chirpUsb);

    // load programs
    exec_addProg(&g_progBlobs);
    exec_addProg(&g_progVideo, true);

    exec_loop();
}
