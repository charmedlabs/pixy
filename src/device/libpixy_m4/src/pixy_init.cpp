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
#include "pixy_init.h"
#ifndef KEIL
#include "cr_start_m0.h"
#endif
#include "platform_config.h"
#include "smlink.hpp"
#include "usblink.h"
#include "param.h"
#include "camera.h"
#include "rcservo.h"
#include "led.h"	  
#include "power.h"
#include "misc.h"

Chirp *g_chirpUsb = NULL;
Chirp *g_chirpM0 = NULL;

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
	timerInit();
	GPIOInit();
	USB_UserInit();

  	debug_frmwrk_init_clk(CLKFREQ);
	lpc_printf("M4 start\n");
}

#define AWB_TIMEOUT   6000*1000  // 6 seconds

void handleAWB()
{
	static uint32_t timer;
	static uint8_t state = 0;
	uint8_t awbp=0;

	if (state==2)
		return;

	prm_get("Auto White Balance on power-up", &awbp, END);
	if (!awbp)
		return; // exit if auto white balance on power-up is disabled

	else if (state==0)
	{
		setTimer(&timer);
		cam_setAWB(1);
		state = 1;
	}
	else if (state==1)
	{
		if (getTimer(timer)>AWB_TIMEOUT)
		{
			cam_setAWB(0);
		 	state = 2; // end state machine (only run once)
		}
	}
}

void periodic()
{
	// check to see if guard data still there
//	if (STACK_GUARD != STACK_GUARD_WORD)
//		showError(1, 0xffff00, "stack corruption\n");

	while(g_chirpUsb->service());
	handleAWB();
}

#ifdef KEIL
void pixyInit(uint32_t slaveRomStart, const unsigned char slaveImage[], uint32_t imageSize)
#else
void pixyInit(void)
#endif
{
	// write stack guard word
 //	STACK_GUARD = STACK_GUARD_WORD;

	commonInit();

#ifdef KEIL
	IPC_haltSlave();
#endif

	// clear RC servo registers to prevent and glitches upon initialization
	rcs_enable(0, 0);
	rcs_enable(1, 0);

	ADCInit();
   	SCTInit();
	CameraInit();

	// initialize shared memory interface before running M0
	SMLink *smLink = new SMLink;

	// start slave
#ifdef KEIL
	if (slaveRomStart && slaveImage && imageSize)
	{
		IPC_downloadSlaveImage(slaveRomStart, slaveImage, imageSize);
		IPC_startSlave();
	}
#else
    cr_start_m0(SLAVE_M0APP,&__core_m0app_START__);
#endif

	// initialize chirp objects
	USBLink *usbLink = new USBLink;
	g_chirpUsb = new Chirp(false, false, usbLink);
	g_chirpUsb->setSendTimeout(3000); // set a high timeout because the host can sometimes go AWOL for a second or two....
	g_chirpUsb->setRecvTimeout(3000); // set a high timeout because the host can sometimes go AWOL for a second or two....

  	g_chirpM0 = new Chirp(false, true, smLink);

	// initialize devices/modules
	led_init();
	prm_init(g_chirpUsb);
	pwr_init();
	cam_init();
}

void pixySimpleInit(void)
{
	commonInit();

	USBLink *usbLink = new USBLink;
	g_chirpUsb = new Chirp(false, false, usbLink);
}

void cprintf(const char *format, ...)
{
    char  buf[128];
    va_list args;
    va_start(args, format);
    vsprintf((char *)buf, (char const *)format, args);
    va_end(args);

	CRP_SEND_XDATA(g_chirpUsb, HSTRING(buf));
}


