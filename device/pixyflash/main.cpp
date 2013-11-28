#include <pixy_init.h>
#include <misc.h>
#include "flashprog.h"
#include "lpc43xx_rgu.h"

#if 1
#define USE_SPIFI_LIB
#include "spifi_rom_api.h"

extern uint8_t g_resetFlag;

#define SPIFI_CMD     (*(volatile uint32_t *)0x40003004)
#define SPIFI_STAT    (*(volatile uint32_t *)0x4000301c)

// initiate a soft reset (run code in spifi) when flag is set. 
void handleReset()
{
	// note, none of this soft reset code works.  There is some kind of issue with the spifi coming out of reset.  
	// 1) if you boot into spifi from power-up and run this code (from ram), reset works.
	//    but only if you do soft reset of RGU_SIG_M3. RGU_SIG_CORE won't work, which is consistent with (3) below. 
	// 2) if you boot into dfu from power-up (non-spifi) and run this code, reset fails.
	// This implies that some state either in the spifi device or the spifi controller isn't being reset/restored correctly 
	// during reset.      
	// 3) if you softreset the spifi, (1) no longer holds. 
	// This implies that the state of the spifi controller is likely causing the reset hang.
	// 4) reading the state of the spifi controller (registers 0x40003000-0x40003020) shows a change in controller
	//    state regarding (1) and (2), namely 0x40003014, 3018, 301c, but strangely, trying to read 0x40003014 programatically
	//    hangs the processor. 
	// There's this:
	// http://www.lpcware.com/content/blog/introduction-spifi
	// which describes how to reset the spifi device. 
	// The Winbond W25Q80BV has a 0xff reset instruction (for stopping a read stream).  
	// But this doesn't help either.   
	// Other sources like the errata:
	// http://www.nxp.com/documents/errata_sheet/ES_LPC43X0_A.pdf
	// say that you need to issue a cancel_mem_mode() command beofore resetting.  This doesn't work either. 
	// http://www.lpcware.com/content/forum/soft-reset-spifi   
	if (g_resetFlag)  
	{
		delayus(100000); // wait for USB return packet to be sent
#if 0
		// reset spifi device
		SPIFI_CMD =(0xffu << 24) | // opcode 0xFF winbond reset
			(0x1 << 21) | // frame form indicating opcode only
			(0x0 << 19) | // field form indicating all serial
			(0); // datalen
		while(SPIFI_STAT & 2); // wait for command to complete
  		//RGU_SoftReset(RGU_SIG_SPIFI); // reset spifi controller
		delayus(100000); // wait for spifi device to reset
#endif
		(*spifi_table.cancel_mem_mode)(&g_spifi);
		(*spifi_table.cancel_mem_mode)(&g_spifi);
		delayus(100000); // wait for spifi device to reset
  		RGU_SoftReset(RGU_SIG_CORE); // reset processor
		while(1);
	}
}
#endif

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

