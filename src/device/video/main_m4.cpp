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

#include <stdio.h>
#include <debug.h>
#include <pixy_init.h>
#include <pixyvals.h>
#include <pixy_init.h>
#include <misc.h>
#include "camera.h"
#include "led.h"
#include "conncomp.h"
#include "exec.h"
#include "camera.h"
#include "progvideo.h"
#include "progblobs.h"
#include "progpt.h"
#include "progchase.h"
#include "param.h"
#include "serial.h"

// M0 code 
const // so m0 program goes into RO memory
#include "m0_image.c"


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
}

int main(void) 
{
	// main init of hardware plus a version-dependent number for the parameters that will
	// force a format of parameter between version numbers.  
 	pixyInit(SRAM3_LOC, &LR0[0], sizeof(LR0), FW_MAJOR_VER*40*40+FW_MINOR_VER*40+FW_BUILD_VER);

	cc_init(g_chirpUsb);
	ser_init();
	exec_init(g_chirpUsb);

	// load programs
	exec_addProg(&g_progBlobs);
	ptLoadParams();
	exec_addProg(&g_progPt);
#if 0
	chaseLoadParams();
	exec_addProg(&g_progChase);
#endif
	exec_addProg(&g_progVideo, true);

	exec_loop();

#if 0
	#define DELAY 1000000
	rcs_setFreq(100);
	rcs_setLimits(0, -200, 200);
	rcs_setLimits(1, -200, 200);
	while(1)
	{
		rcs_setPos(0, 0);
		delayus(DELAY);
		rcs_setPos(0, 500);
		delayus(DELAY);
		rcs_setPos(0, 1000);
		delayus(DELAY);
		rcs_setPos(1, 0);
		delayus(DELAY);
		rcs_setPos(1, 500);
		delayus(DELAY);
		rcs_setPos(1, 1000);
		delayus(DELAY);
	}

#endif
#if 0
	while(1)
	{
		g_chirpUsb->service();
		handleButton();
	}
#endif
}

