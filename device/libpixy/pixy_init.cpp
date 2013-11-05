#include "pixy_init.h"
#include "platform_config.h"
#include "camera.h"
#include "rcservo.h"
#include "led.h"	  
#include "power.h"
#include "spi.h"

ChirpUsb *g_chirpUsb = NULL;
ChirpM0 *g_chirpM0 = NULL;

void ADCInit()
{
	ADC_Init(LPC_ADC0, 200000, 10);
	ADC_IntConfig(LPC_ADC0,ADC_ADINTEN1,DISABLE);
}

void SCTInit()
{
	// RC servo
	LPC_SCT->CTRL_L |= 1<<2; // set halt 
	LPC_SCT->CTRL_L &= ~(255<<5); // reset prescaler
	LPC_SCT->CTRL_L |= 203<<5; // prescale (create a 1 MHz clock)
	LPC_SCT->LIMIT_L = 1<<0; // event 0 resets counter

  	LPC_SCT->OUTPUT = 0x0;
	LPC_SCT->CONFIG &= ~0x01; // set UNITY=0, default clk source, bus clock

	LPC_SCT->MATCH[0].L = 20000; // 4000;
	LPC_SCT->MATCHREL[0].L = 20000; // 4000;
	LPC_SCT->EVENT[0].CTRL = 0 | 1<<12;
	LPC_SCT->EVENT[1].CTRL = 1 | 1<<12;
	LPC_SCT->EVENT[0].STATE = 1<<0; // event 0 is enabled in state 0
	LPC_SCT->EVENT[1].STATE = 1<<0; // event 1 is enabled in state 0

	LPC_SCT->EVENT[2].CTRL = 2 | 1<<12;
	LPC_SCT->EVENT[2].STATE = 1<<0; // event 2 is enabled in state 0

	// RGB led
	LPC_SCT->CTRL_H |= 1<<2; // set halt 
	LPC_SCT->CTRL_H &= ~(255<<5); // reset prescaler
	LPC_SCT->CTRL_H |= 9<<5; // prescale (create a 1 MHz clock)
	LPC_SCT->LIMIT_H = 1<<3; // event 3 resets counter

	LPC_SCT->MATCH[0].H = 0xffff;
	LPC_SCT->MATCHREL[0].H = 0xffff;
	LPC_SCT->EVENT[3].CTRL = 0 | 1<<4 | 1<<12;
	LPC_SCT->EVENT[3].STATE = 1<<0; // event 3 is enabled in state 0

	// red
	LPC_SCT->EVENT[4].CTRL = 1 | 1<<4 | 1<<12;
	LPC_SCT->EVENT[4].STATE = 1<<0; // event 4 is enabled in state 0

	// green
	LPC_SCT->EVENT[5].CTRL = 2 | 1<<4 | 1<<12;
	LPC_SCT->EVENT[5].STATE = 1<<0; // event 5 is enabled in state 0

	// blue
	LPC_SCT->EVENT[6].CTRL = 3 | 1<<4 | 1<<12;
	LPC_SCT->EVENT[6].STATE = 1<<0; // event 6 is enabled in state 0

	// start SCT timers
	LPC_SCT->CTRL_L &= ~(1<<2);
	LPC_SCT->CTRL_H &= ~(1<<2); 
}


void CameraInit(void)
{
	volatile uint32_t d;

	LPC_GPIO_PORT->MASK[0] = 0;
	LPC_GPIO_PORT->MASK[1] = 0;
	LPC_GPIO_PORT->DIR[0] = 0x0007;
	LPC_GPIO_PORT->DIR[1] = 0x0000;

	// deal with pwdn
	LPC_GPIO_PORT->PIN[0] = 0;
	LPC_GPIO_PORT->PIN[0] |= 0x0004;
	for (d=0; d<10000000; d++);
	LPC_GPIO_PORT->PIN[0] &= ~(0x0004);

}

void GPIOInit(void)
{
	// button, SPI_SSEL
	LPC_GPIO_PORT->MASK[5] = 0;
	LPC_GPIO_PORT->PIN[5] = 0x20; // negate SPI_SS
	LPC_GPIO_PORT->DIR[5] = 0x20; // SPI_SS - output

	// deal with P4_1, GPIO2[1]
	LPC_GPIO_PORT->MASK[2] = 0;
	LPC_GPIO_PORT->DIR[2] = 0;
}

void timerInit(void)
{
	// set timer so we count clock cycles
	LPC_TIMER1->IR = 0;
 	LPC_TIMER1->TCR = 1;
	LPC_TIMER1->PR = 0;

	// microsecond timer
	LPC_TIMER2->IR = 0;
 	LPC_TIMER2->TCR = 1;
	LPC_TIMER2->PR = CLKFREQ_US-1;
}

void commonInit(void)
{
	platformInit();
	GPIOInit();
	timerInit();
	USB_UserInit();

  	debug_frmwrk_init_clk(CLKFREQ);
	lpc_printf("M4 start\n");
}

void pixyInit(uint32_t slaveRomStart, const unsigned char slaveImage[], uint32_t imageSize)
{
	commonInit();

	IPC_haltSlave();

	ADCInit();
   	SCTInit();
	CameraInit();

	// start slave
	if (slaveRomStart && slaveImage && imageSize)
	{
		IPC_downloadSlaveImage(slaveRomStart, slaveImage, imageSize);
		IPC_startSlave();
	}

	// initialize chirp objects
	g_chirpUsb = new ChirpUsb();
  	g_chirpM0 = new ChirpM0();

	// initialize devices/modules
	pwr_init();
	spi_init();
	cam_init();
	rcs_init();
	led_init();
	//cc_init();
}

void pixySimpleInit(void)
{
	commonInit();

	g_chirpUsb = new ChirpUsb();
}

