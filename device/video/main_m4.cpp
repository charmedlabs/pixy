#include <stdio.h>
#include <debug.h>
#include <pixy_init.h>
#include <pixyvals.h>
#include <pixy_init.h>
#include <misc.h>
#include <string.h>
#include <new>
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
#include "lpc43xx.h"
#include "lpc43xx_scu.h"
#include "lpc43xx_uart.h"


// M0 code 
const // so m0 program goes into RO memory
#include "m0_image.c"

#if 0
extern "C" void UART0_IRQHandler(void);

void UART0_IRQHandler(void)
{
	uint32_t status;
	volatile uint32_t v;

	/* Determine the interrupt source */
	status = LPC_USART0->IIR & UART_IIR_INTID_MASK;


	// Receive Data Available 
	if (status==UART_IIR_INTID_RDA || status==UART_IIR_INTID_CTI)
	{
		v = LPC_USART0->RBR&UART_RBR_MASKBIT;
		printf("%d\n", v);

	}

	// Transmit Holding Empty
	if (status==UART_IIR_INTID_THRE)
	{
		printf("tre\n");
	}
}
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
}
 
int main(void) 
{
 	pixyInit(SRAM3_LOC, &LR0[0], sizeof(LR0));

	cc_init(g_chirpUsb);
	ser_init();
	exec_init(g_chirpUsb);

#if 0
	scu_pinmux(0x2, 0, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // U0_TXD 
	scu_pinmux(0x2, 1, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // U0_RXD
 	scu_pinmux(0x1, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	         // turn SSP1_MISO into GPIO0[10]
	//scu_pinmux(0x2, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // gpio5[4] 
	scu_pinmux(0x1, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	         // turn SSP1_MOSI into GPIO0[11]
	//scu_pinmux(0x2, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // gpio5[3] 
 

	UART_CFG_Type ucfg;
	UART_FIFO_CFG_Type ufifo;

	ucfg.Baud_rate = 19200;
	ucfg.Databits = UART_DATABIT_8;
	ucfg.Parity = UART_PARITY_NONE;
	ucfg.Stopbits = UART_STOPBIT_1;
	ucfg.Clock_Speed = CLKFREQ;

	ufifo.FIFO_DMAMode = DISABLE;
	ufifo.FIFO_Level = UART_FIFO_TRGLEV0;
	ufifo.FIFO_ResetRxBuf = ENABLE;
	ufifo.FIFO_ResetTxBuf = ENABLE;

	UART_Init(LPC_USART0, &ucfg);
	UART_FIFOConfig(LPC_USART0, &ufifo);
	UART_TxCmd(LPC_USART0, ENABLE);

	UART_IntConfig(LPC_USART0, UART_INTCFG_RBR, ENABLE);
	UART_IntConfig(LPC_USART0, UART_INTCFG_THRE, ENABLE);

	//UART_Send(LPC_USART0, "hello", 6, BLOCKING);
	uint8_t buf;
	volatile uint32_t v;

    NVIC_SetPriority(USART0_IRQn, 0);
	/* Enable Interrupt for UART0 channel */
    NVIC_EnableIRQ(USART0_IRQn);

#if 0
	while(1)
	{
        //UART_Receive(LPC_USART0, &buf, 1, BLOCKING);
		if(LPC_USART0->LSR & UART_LSR_RDR)
			printf("%d\n", LPC_USART0->RBR & UART_RBR_MASKBIT);
	}
#endif
#if 1
	while(1)
	{
		buf = 123;
		UART_Send(LPC_USART0, &buf, 1, BLOCKING);
		delayus(100000);
	} 
#endif
#endif

#if 0
	i2c_init();
	while(1);
#endif
#if 0
	exec_addProg(&g_progVideo);
	exec_loop();
#endif  

#if 1
	exec_addProg(&g_progBlobs);
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
		//uint8_t *buf = new (std::nothrow) uint8_t[0x1000];
		uint8_t *buf = new uint8_t[0x1000];
	}

#endif	
#if 0
	uint32_t *memory = (uint32_t *)RLS_MEMORY;
	int result;

   	// get first frame (primer)
	while(1)
		result = cc_getRLSFrame(memory, LUT_MEMORY, &g_numRls);
#endif
#if 0
	// spi I/O test
	scu_pinmux(0x1, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	         // SSP1_MISO 
	scu_pinmux(0x1, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	         // SSP1_MOSI 
	LPC_GPIO_PORT->MASK[0] = 0;
	LPC_GPIO_PORT->PIN[0] = 0x0;
	LPC_GPIO_PORT->DIR[0] = 0xc00;

	while(1)
	{
		LPC_GPIO_PORT->PIN[0] = 0x400;	// pin 1 high
		LPC_GPIO_PORT->PIN[0] = 0xc00;	// pin 4 high
		LPC_GPIO_PORT->PIN[0] = 0x800;
		LPC_GPIO_PORT->PIN[0] = 0x000;
	}
#endif
#if 0
	// uart I/O test
	LPC_GPIO_PORT->MASK[5] = 0;
	LPC_GPIO_PORT->PIN[5] = 0x0;
	LPC_GPIO_PORT->DIR[5] = 0x3;

	while(1)
	{
		LPC_GPIO_PORT->PIN[5] = 0x01;	// pin 4 high
		LPC_GPIO_PORT->PIN[5] = 0x03;	// pin 1 high
		LPC_GPIO_PORT->PIN[5] = 0x02;
		LPC_GPIO_PORT->PIN[5] = 0x00;
	}
#endif
#if 0
	// dac test
	LPC_DAC->CTRL =	8;
	LPC_DAC->CR = 0 << 6;
	LPC_DAC->CR = 100 << 6;
	LPC_DAC->CR = 200 << 6;
	LPC_DAC->CR = 300 << 6;
	LPC_DAC->CR = 400 << 6;
	LPC_DAC->CR = 500 << 6;
	LPC_DAC->CR = 600 << 6;
	LPC_DAC->CR = 700 << 6;
	LPC_DAC->CR = 800 << 6;
	LPC_DAC->CR = 900 << 6;
	LPC_DAC->CR = 1000 << 6;
#endif
#if 0
   	while(1)
	{
		delayus(100000);
		g_chirpUsb->assemble(0, HSTRING("hello\n"), END);
		g_chirpUsb->service();
	}
#endif
#if 0
	SPIFIopers spifi;
	memset((void *)&spifi, 0, sizeof(spifi));
	char datab[4] = {0, 0x12, 0x34, 0x56};
	spifi.dest = (char *)g_spifi.base;
	spifi.length = g_spifi.memSize;
	spifi.scratch = NULL;
	spifi.options = S_VERIFY_ERASE;

	if (spifi_erase(&g_spifi, &spifi)) 
		return 0;
	/* Setup SPI FLASH operation via the SPI FLASH driver */
	spifi.dest = (char *)g_spifi.base;
	spifi.length = 4;
	spifi.scratch = (char *) NULL;
	spifi.protect = 0;
	spifi.options = S_CALLER_ERASE;

	if (spifi_program(&g_spifi, datab, &spifi))
		return 0;

	while(1);
#endif
#if 0	
	uint32_t a = 0xffffffff;
	uint32_t b = 0;
	uint32_t c = b - a;

	printf("*** %d\n", c);
#endif
#if 0
	uint32_t i = 0;
	uint32_t timer;
	setTimer(&timer);
	while(1)
	{
		if (getTimer(timer)>1000000)
		{
			printf("%d\n", i++);
			setTimer(&timer);
		}
	}
#endif
#if 0

	uint16_t buf[16];

	spi_setCallback(transmitCallback);

 	while(1)
	{
		
		if (spi_receive(buf, 1))
		{
			printf("%x\n", buf[0]);
			if (buf[0]!=0xa5a5)
				spi_sync(); 
		}
	}

#endif
#if 0
	// 0 pan, 0 to 1000 clockwise
	// 1 tilt, 0 to 1000 tilt up
	while(1)
	{
		// bring up
		servoMove(1, 650, 900, r(800, 50), 0, 550, 450);
		servoMove(1, 900, 1000, 600, 0, 450, 350);
		servoMove(1, 1000, 1025, 100, 0, 350, r(320, 20));
		servoMove(1, 1025, 1000, 150, 0, 320, r(360, 20));
		delayus(2000000);
		// bring down
		servoMove(1, 1000, 900, r(600, 50), 0, 350, 460);
		servoMove(1, 900, 650, r(850, 50), 0, 450, 550);
		servoMove(1, 650, 670, 100, 0, 550, r(520, 20));
		servoMove(1, 670, 650, 150, 0, 520, r(550, 20));
		delayus(1500000);

		// test 1
		servoMove(1, 650, 900, r(800, 50), 0, 550, 450);
		servoMove(1, 900, 1000, 600, 0, 450, 350);
		servoMove(1, 1000, 1025, 100, 0, 350, r(320, 20));
		servoMove(1, 1025, 1000, 150, 0, 320, r(360, 20));
		servoMove(1, 1000, 900, r(600, 50), 0, 350, 460);
		servoMove(1, 900, 650, r(850, 50), 0, 450, 550);
		servoMove(1, 650, 670, 100, 0, 550, r(520, 20));
		servoMove(1, 670, 650, 150, 0, 520, r(550, 20));

		// test 2
		servoMove(1, 650, 900, r(800, 50), 0, 550, 450);
		servoMove(1, 900, 1000, 600, 0, 450, 350);
		servoMove(1, 1000, 1025, 100, 0, 350, r(320, 20));
		servoMove(1, 1025, 1000, 150, 0, 320, r(360, 20));
		servoMove(1, 1000, 900, r(600, 50), 0, 350, 460);
		servoMove(1, 900, 650, r(850, 50), 0, 450, 550);
		servoMove(1, 650, 670, 100, 0, 550, r(520, 20));
		servoMove(1, 670, 650, 150, 0, 520, r(550, 20));
		delayus(5000000);

	}
#endif   	
#if 0
	while(1)
	{
		g_chirpUsb->service();
		handleButton();
	}
#endif
#if 0
	g_chirpM0->getProc("getRLSFrame", (ProcPtr)getRLSFrameCallback);

   	blobProcess();

#endif

#if 0
#define SERVO

	int32_t result, row;
	uint32_t i, j, numRls, startCol, length, model, xsum, ysum, n, xavg, yavg;
 	uint32_t *qVals = (uint32_t *)RLS_MEMORY;

	//motor(0, 0);
	// to switch between servo and motor--
	// uncomment servo or motor below, respectively
	// for motor, change pixy_init.cpp, SCT init
	// LPC_SCT->MATCH[0].L = 4000;
	// LPC_SCT->MATCHREL[0].L = 4000;
	// this will increase the pwm freq and reduce the latency
	// (these values are normally 20000)
	// Servo connectors --- black wire down, yellow up.
	// tilt: edge
	// pan: inner
	// note, tilt servo has wire facing forward

	j = 0;
	while(1)
	{
		g_chirpUsb->service();
		handleButton();
		if (g_loop)
		{

			//cc_getRLSFrame(qVals, RLS_MEMORY_SIZE, LUT_MEMORY, &numRls);
			cc_getMaxBlob(NULL);

#if 0
    		for (i=0, row=-1, n=0, xsum=0, ysum=0; i<numRls; i++)
    		{
        		if (qVals[i]==0)
        		{
            		row++;
            		continue;
        		}
        		model = qVals[i]&0x03;
        		qVals[i] >>= 3;
        		startCol = qVals[i]&0x1ff;
        		qVals[i] >>= 9;
        		length = qVals[i]&0x1ff;
				xsum += startCol + (length>>1);
				ysum += row;
				n++;
			}
			if (n>15)
			{
				xavg = xsum/n;
				yavg = ysum/n;
			}
			else
			{
				xavg = XCENTER;
#ifdef SERVO
				yavg = YCENTER;
#else
			 	yavg = YTRACK;
#endif
			}
#ifdef SERVO
			servo(xavg, yavg);
#else
			motor(xavg, yavg);
#endif
//		   	printf("%d %d\n", xavg, yavg);
#endif
		if (j%50==0)
			printf("%d\n", j);
		j++;   			
		}				
    }
#endif
}

