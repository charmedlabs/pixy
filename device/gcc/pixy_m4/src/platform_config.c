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

#include "LPC43xx.h"
#include "platform_config.h"

#include "lpc43xx_scu.h"
#include "lpc43xx_cgu.h"
#include "lpc_types.h"
#include "spifi_rom_api.h"

SPIFIobj g_spifi;

/**********************************************************************
 ** Function prototypes
 **********************************************************************/
void vIOInit(void);
void clockInit(void);
void fpuInit(void);



/* this function initializes the platform with system level settings */
void platformInit(void) {

	SystemInit();	
	
	/* checks for presence of an FPU unit */
	fpuInit();

	clockInit();
	vIOInit();

	#if (USE_EXT_STATIC_MEM == YES) || (USE_EXT_DYNAMIC_MEM == YES)
	 
	EMC_Init();
	
	#endif

    #if (USE_EXT_FLASH == YES)
	
	// relocate vector table to internal ram
	// updates also VTOR
	relocIrqTable(); 
	
	#endif

}

/*----------------------------------------------------------------------------
  Initialize board specific IO
 *----------------------------------------------------------------------------*/
void vIOInit(void)
{	
	// disable clocks to peripherals we don't use	
	LPC_CCU1->CLK_APB3_I2C1_CFG = 0;
	LPC_CCU1->CLK_APB3_ADC1_CFG = 0;
	LPC_CCU1->CLK_APB3_CAN0_CFG = 0;
	LPC_CCU1->CLK_APB1_MOTOCONPWM_CFG = 0;
	LPC_CCU1->CLK_APB1_I2S_CFG = 0;
	LPC_CCU1->CLK_APB1_CAN1_CFG = 0;
	LPC_CCU1->CLK_M4_LCD_CFG = 0;
	LPC_CCU1->CLK_M4_ETHERNET_CFG = 0;
	LPC_CCU1->CLK_M4_EMC_CFG = 0;
	LPC_CCU1->CLK_M4_SDIO_CFG = 0;
	LPC_CCU1->CLK_M4_USB1_CFG = 0;
	LPC_CCU1->CLK_M4_EMCDIV_CFG = 0;

	LPC_CCU1->CLK_M4_USART2_CFG = 0;
	LPC_CCU1->CLK_M4_USART3_CFG = 0;

	LPC_CCU1->CLK_M4_SSP0_CFG = 0;
	LPC_CCU1->CLK_M4_QEI_CFG = 0;

	LPC_CCU2->CLK_APB0_SSP0_CFG = 0;
	LPC_CCU2->CLK_APB2_USART3_CFG = 0;
	LPC_CCU2->CLK_APB2_USART2_CFG = 0;
	LPC_CCU2->CLK_SDIO_CFG = 0;

	scu_pinmux(0x0, 0,  (MD_PUP | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio0[0] siod
	scu_pinmux(0x0, 1,  (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio0[1] sioc
	scu_pinmux(0x1, 15, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio0[2] PWDN
	scu_pinmux(0x1, 16, (MD_PUP | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio0[3] RSTB

	scu_pinmux(0x1, 7,  (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0);          // gpio1[0] Y0 
	scu_pinmux(0x1, 8,  (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0);          // gpio1[1] Y1
	scu_pinmux(0x1, 9,  (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0);          // gpio1[2] Y2
	scu_pinmux(0x1, 10, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0);          // gpio1[3] Y3
	scu_pinmux(0x1, 11, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[4] Y4
	scu_pinmux(0x1, 12, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[5] Y5
	scu_pinmux(0x1, 13, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[6] Y6
	scu_pinmux(0x1, 14, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[7] Y7
	scu_pinmux(0x1, 5,  (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[8] Y8
	scu_pinmux(0x1, 6,  (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[9] Y9
	scu_pinmux(0x2, 11, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[11] HSYNC
	scu_pinmux(0x2, 12, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[12] VSYNC
	scu_pinmux(0x2, 13, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio1[13] PCLK
	scu_pinmux(0x6, 5, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // CTOUT_6 RCS0	CHANGED FROM FUNC1 TO FUNC0
	scu_pinmux(0x6, 12, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	     // CTOUT_7 RCS1

	scu_pinmux(0x4, 2, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // CTOUT_0 RED		ORIGINALLY FUNC1
	scu_pinmux(0x2, 7, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // CTOUT_1 GREEN	ORIGINALLY FUNC1
	scu_pinmux(0x2, 10, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	     // CTOUT_2 BLUE		ORIGINALLY FUNC1

	scu_pinmux(0x3, 1, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // gpio5[8] VBUS_EN

	scu_pinmux(0x5, 6, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // U1_TXD (output)
	scu_pinmux(0x5, 7, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // U1_RXD (input)
	scu_pinmux(0x2, 0, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // U0_TXD 
	scu_pinmux(0x2, 1, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // U0_RXD

	scu_pinmux(0x1, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC5); 	         // SSP1_MISO 
	scu_pinmux(0x1, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC5); 	         // SSP1_MOSI 
	scu_pinmux(0x1, 19, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // SSP1_SCK 
	scu_pinmux(0x1, 20, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // SSP1_SSEL 
	scu_pinmux(0x2, 2, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // gpio5[2] 
	scu_pinmux(0x2, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // gpio5[3] 
	scu_pinmux(0x2, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // gpio5[4] 
	scu_pinmux(0x2, 5, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); // FUNC4 	         // gpio5[5] rev 1.1 SS control
	scu_pinmux(0x3, 2, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // gpio5[9]
	
	scu_pinmux(0x2, 8, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // push-button gpio5[7]
	scu_pinmux(0x4, 1, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	         // gpio2[1] rev 1.1 


	//scu_pinmux(0x7, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio4[9] 
	//scu_pinmux(0x7, 5, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio4[9] 
	//scu_pinmux(0x7, 6, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio4[9] 
	//scu_pinmux(0x7, 7, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	     // gpio4[9] 

	LPC_SCU_CLK(0) = 1 + (MD_PLN | MD_EZI | MD_ZI | MD_EHS); /*  EXTBUS_CLK0  IDIVB input */
}
	
/*----------------------------------------------------------------------------
  Initialize clocks
 *----------------------------------------------------------------------------*/


#define __CRYSTAL        (12000000UL)    /* Crystal Oscillator frequency */

static void delayus(uint32_t us)
{
	volatile uint32_t i, j;	
	
	for (i=0; i<us; i++)
		for (j=0; j<38; j++);
}

void clockInit(void)
{
	uint32_t EMCClk;

	__disable_irq();
 	/* Set the XTAL oscillator frequency to 12MHz*/
	CGU_SetXTALOSC(__CRYSTAL);
	CGU_EnableEntity(CGU_CLKSRC_XTAL_OSC, ENABLE);
	CGU_EntityConnect(CGU_CLKSRC_XTAL_OSC, CGU_BASE_M3);
	
	/* Set PL160M 12*1 = 12 MHz */
	CGU_EntityConnect(CGU_CLKSRC_XTAL_OSC, CGU_CLKSRC_PLL1);
//	CGU_EntityConnect(CGU_CLKSRC_IRC, CGU_CLKSRC_PLL1);
	CGU_SetPLL1(1);
	CGU_EnableEntity(CGU_CLKSRC_PLL1, ENABLE);

	// setup CLKOUT
	CGU_EntityConnect(CGU_CLKSRC_PLL1, CGU_CLKSRC_IDIVB);
	CGU_EnableEntity(CGU_CLKSRC_IDIVB, ENABLE);
	CGU_SetDIV(CGU_CLKSRC_IDIVB, 12);  // 12 -> 6 pclks per cpu clk, 10 -> 5 pclks
	// set input for CLKOUT to IDIVB
	LPC_CGU->BASE_OUT_CLK &= ~0x0f000000;
	LPC_CGU->BASE_OUT_CLK |= 0x0d000000;

	/* Run SPIFI from PL160M, /2 */
	CGU_EntityConnect(CGU_CLKSRC_PLL1, CGU_CLKSRC_IDIVA);
	CGU_EnableEntity(CGU_CLKSRC_IDIVA, ENABLE);
	CGU_SetDIV(CGU_CLKSRC_IDIVA, 2);
	CGU_EntityConnect(CGU_CLKSRC_IDIVA, CGU_BASE_SPIFI);
	CGU_UpdateClock();

	LPC_CCU1->CLK_M4_EMCDIV_CFG |=    (1<<0) |  (1<<5);		// Turn on clock / 2
	LPC_CREG->CREG6 |= (1<<16);	// EMC divided by 2
    LPC_CCU1->CLK_M4_EMC_CFG |= (1<<0);		// Turn on clock

	/* Set PL160M @ 12*9=108 MHz */
	CGU_SetPLL1(9);

	/* Run base M3 clock from PL160M, no division */
	CGU_EntityConnect(CGU_CLKSRC_PLL1, CGU_BASE_M3);

	delayus(10000);

	/* Change the clock to 204 MHz */
	/* Set PL160M @ 12*15=180 MHz */
	CGU_SetPLL1(17);

	delayus(10000);

	CGU_UpdateClock();

	LPC_SCU->SFSP3_3 = 0xF3; /* high drive for SCLK */
	/* IO pins */
	LPC_SCU->SFSP3_4=LPC_SCU->SFSP3_5=LPC_SCU->SFSP3_6=LPC_SCU->SFSP3_7 = 0xD3;
	LPC_SCU->SFSP3_8 = 0x13; /* CS doesn't need feedback */

	EMCClk = CGU_GetPCLKFrequency(CGU_PERIPHERAL_SPIFI)/1000000;
	if (spifi_init(&g_spifi, EMCClk/5, S_RCVCLK | S_FULLCLK, EMCClk))
		while(1);

	__enable_irq();
}


