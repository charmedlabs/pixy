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

#include <pixy_init.h>
#include <misc.h>
#include "flashprog.h"
#include "lpc43xx_scu.h"
#include "lpc43xx_rgu.h"

#define USE_SPIFI_LIB
#include "spifi_rom_api.h"

extern uint8_t g_resetFlag;

#define SPIFI_CMD     (*(volatile uint32_t *)0x40003004)
#define SPIFI_STAT    (*(volatile uint32_t *)0x4000301c)

// initiate a soft reset (run code in spifi) when flag is set. 
void handleReset()
{
	if (g_resetFlag)  
	{
	  	scu_pinmux(0x2, 7, (MD_PUP | MD_EZI | MD_ZI | MD_EHS), FUNC0);
		delayus(100000); // wait for USB return packet to be sent
		// reset spifi device
		SPIFI_CMD =(0xffu << 24) | // opcode 0xFF winbond reset
			(0x1 << 21) | // frame form indicating opcode only
			(0x0 << 19) | // field form indicating all serial
			(0); // datalen
		while(SPIFI_STAT & 2); // wait for command to complete
  		//RGU_SoftReset(RGU_SIG_SPIFI); // reset spifi controller
		delayus(100000); // wait for spifi device to reset
  		RGU_SoftReset(RGU_SIG_CORE); // reset processor
		while(1);
	}
}

int main(void) 
 {	
  	pixySimpleInit();
	flash_init();
	while(1)
	{
		g_chirpUsb->service();
		handleReset();
	}
}

