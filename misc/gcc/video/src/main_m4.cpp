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
#include "param.h"
#include "serial.h"
#include "flash_config.h"
#include "spifi_rom_api.h"

#include <cr_section_macros.h>

#if defined (LPC43_MULTICORE_M0APP) | defined (LPC43_MULTICORE_M0SUB)
#include "cr_start_m0.h"
#endif

// M0 code 
//const // so m0 program goes into RO memory
//#include "m0_image.c"


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

#if 0
	pixyInit();

	cc_init(g_chirpUsb);
	ser_init();
	exec_init(g_chirpUsb);
#endif

#if 1		/* test loop */
	pixyInit();
	exec_init(g_chirpUsb);
#if 0
	int i = 0;
	cam_setMode(1);
	while(1)
	{
		//uint8_t reg = cam_getRegister(0x0a);
		g_chirpUsb->service();
		cprintf("hello world %d\n", i++);
	}
#endif
#if 0
	while(1)
	{
		uint8_t *frame = (uint8_t *)SRAM1_LOC;
		int res;

		res = cam_getFrame(frame, SRAM1_SIZE, CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT);
		i++;
		if (i%50==0)
		{
			lpc_printf("%d\n", i);
		}

	}
#endif
#endif

#if 1
	exec_addProg(&g_progBlobs);
	ptLoadParams();
	exec_addProg(&g_progPt);
	exec_addProg(&g_progVideo, true);
	exec_loop();
#endif

#if 0

	//prm_format();
	ColorModel model, *model2;
	uint32_t len;
	model.m_hue[0].m_slope = 1.0;
	model.m_hue[0].m_yi = 2.0;
	model.m_hue[1].m_slope = 3.0;
	model.m_hue[1].m_yi = 4.0;
	model.m_sat[0].m_slope = 5.0;
	model.m_sat[0].m_yi = 6.0;
	model.m_sat[1].m_slope = 7.0;
	model.m_sat[1].m_yi = 8.0;
	prm_add("signature1", "Color signature 1", INTS8(sizeof(ColorModel), &model), END);
	prm_set("signature1", INTS8(sizeof(ColorModel), &model), END);
	model.m_hue[0].m_slope = 9.0;
	model.m_hue[0].m_yi = 10.0;
	model.m_hue[1].m_slope = 11.0;
	model.m_hue[1].m_yi = 12.0;
	model.m_sat[0].m_slope = 13.0;
	model.m_sat[0].m_yi = 14.0;
	model.m_sat[1].m_slope = 15.0;
	model.m_sat[1].m_yi = 16.0;
	prm_add("signature2", "Color signature 2", INTS8(sizeof(ColorModel), &model), END);
	prm_set("signature2", INTS8(sizeof(ColorModel), &model), END);
	prm_get("signature1", &len, &model2, END);
	model.m_hue[0].m_slope = 17.0;
	model.m_hue[0].m_yi = 18.0;
	model.m_hue[1].m_slope = 19.0;
	model.m_hue[1].m_yi = 20.0;
	model.m_sat[0].m_slope = 21.0;
	model.m_sat[0].m_yi = 22.0;
	model.m_sat[1].m_slope = 23.0;
	model.m_sat[1].m_yi = 24.0;
	prm_get("signature1", &len, &model2, END);

	prm_set("signature1", INTS8(sizeof(ColorModel), &model), END);
	prm_get("signature1", &len, &model2, END);
	prm_get("signature2", &len, &model2, END);
	 

#endif
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

