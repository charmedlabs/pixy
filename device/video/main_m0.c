#include <debug.h>
#include <chirp.h>
#include <cycletimer.h>
#include <pixyvals.h>
#include "exec_m0.h"
#include "frame_m0.h"
#include "rls_m0.h"


int main(void)
{
	//CTIMER_DECLARE();
#if 0
	uint32_t memory = SRAM0_LOC;
	uint32_t lut = SRAM0_LOC;

	//while(1);
	memset((void *)QQ_LOC, 0x01, 0x3000);
	g_qqueue->writeIndex = 0;
	g_qqueue->produced = 0;
	g_qqueue->consumed = 0;

 	while(1)
 		getRLSFrame(&memory, &lut); 
#endif
	//printf("M0 start\n");

	chirpOpen();
	exec_init();
	frame_init();
	rls_init();

	//printf("M0 ready\n");
	exec_loop();

#if 0
	while(1)
	{
		CTIMER_START();
		syncM1((uint32_t *)&LPC_GPIO_PORT->PIN[1], 0x2000);
		CTIMER_STOP();
		
		printf("%d\n", CTIMER_GET());
	}	
#endif
#if 0
{
	uint32_t i;
	uint8_t *lut = (uint8_t *)SRAM0_LOC + 0x10000;
	uint32_t memory = SRAM0_LOC;
	uint32_t size = SRAM0_SIZE/2;
	for (i=0; i<0x10000; i++)
		lut[i] = 0;
	lut[0xb400] = 0;
	lut[0xb401] = 1;
	lut[0xb402] = 1;
	lut[0xb403] = 1;
	lut[0xb404] = 0;
	lut[0xb405] = 1;
	lut[0xb406] = 1;
	lut[0xb407] = 0;
	lut[0xb408] = 0;
	lut[0xb409] = 0;

	while(1)
 		getRLSFrame(&memory, &size, (uint32_t *)&lut);
}
#endif
}
