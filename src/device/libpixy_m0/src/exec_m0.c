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

#include "debug.h"
#include <pixyvals.h>
#include "chirp.h"
#include "exec_m0.h"
#include "rls_m0.h"
#include "qqueue.h"

uint8_t g_running = 0;
uint8_t g_run = 0;
int8_t g_program = -1;

int exec_init(void)
{
    chirpSetProc("run", (ProcPtr)exec_run);
    chirpSetProc("stop", (ProcPtr)exec_stop);
    chirpSetProc("running", (ProcPtr)exec_running);

    return 0;
}

uint32_t exec_running(void)
{
    return (uint32_t)g_running;
}

int32_t exec_stop(void)
{
    g_run = 0;
    return 0;
}

int32_t exec_run(uint8_t *prog)
{
    g_program = *prog;
    g_run = 1;
    g_running = 1;
    return 0;
}

#define LUT_MEMORY_SIZE     0x1000 // bytes

void setup0()
{
}

uint32_t g_m0mem = SRAM1_LOC + SRAM1_SIZE - LUT_MEMORY_SIZE - 0x1000;  // 4K should be enough for scratch mem (320/3+2)*8 + 320*8 = 3424
uint32_t g_lut = SRAM1_LOC + SRAM1_SIZE - LUT_MEMORY_SIZE;

void loop0()
{
//  g_qqueue->produced = g_qqueue->consumed = g_qqueue->writeIndex = 0;
    getRLSFrame(&g_m0mem, &g_lut);
}


void exec_loop(void)
{
#if 0
    uint16_t i = 0;
    while(1)
    {
        while(!g_run)
            chirpService();

        setup0();
        while(g_run)
        {
            loop0();
            i++;
            if (i%50==0)
            {
                _DBD16(i); _DBG("\n");
            }
        }
        // set variable to indicate we've stopped
        g_running = 0;
    }
#endif
#if 1
    while(1)
    {
        while(!g_run)
            chirpService();

        setup0();
        while(g_run)
        {
            loop0();
            chirpService();
        }
        // set variable to indicate we've stopped
        g_running = 0;
    }
#endif
}
