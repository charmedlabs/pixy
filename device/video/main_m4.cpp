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
#include "rcservo.h"
#include "spi.h"
#include "spifi_rom_api.h"
#include "lpc43xx_scu.h"
#include "camera.h"
#include "progvideo.h"
#include "progblobs.h"
#include "progpt.h"
#include "param.h"


#define SERVO

// M0 code 
const // so m0 program goes into RO memory
#include "m0_image.c"

extern int g_loop;

#define XCENTER 160			   
#define YCENTER 100
#define YTRACK  160
#define SERVO_CENTER 500
#define SERVO_MAX    1000
#define SERVO_MIN    0

#if 0
class ServoLoop
{
public:
	ServoLoop(uint8_t axis, uint32_t pgain, uint32_t dgain);

	void update(int32_t error);
	void reset();

private:
	int32_t m_pos;
	int32_t m_prevError;
	uint8_t m_axis;
	int32_t m_pgain;
	int32_t m_dgain;
};


ServoLoop::ServoLoop(uint8_t axis, uint32_t pgain, uint32_t dgain)
{
	m_pos = SERVO_CENTER;
	m_axis = axis;
	m_pgain = pgain;
	m_dgain = dgain;
	m_prevError = 0x80000000;
}

void ServoLoop::update(int32_t error)
{
	int32_t vel;

	if (m_prevError!=0x80000000)
	{	
		vel = (error*m_pgain + (error - m_prevError)*m_dgain)/1000;
		m_pos += vel;
		if (m_pos>SERVO_MAX) 
			m_pos = SERVO_MAX; 
		else if (m_pos<SERVO_MIN) 
			m_pos = SERVO_MIN;

		rcs_setPos(m_axis, m_pos);
		//printf("%d %d %d\n", m_axis, m_pos, vel);
	}
	m_prevError = error;
}
void ServoLoop::reset()
{
	m_pos = SERVO_CENTER;
	rcs_setPos(m_axis, m_pos);
}

void servo(uint32_t x, uint32_t y)
{
	static ServoLoop xloop(0, 400, 1000);
	static ServoLoop yloop(1, 400, 1000);
	static int lastLoop = 0;
	int32_t xerror, yerror;
#if 1
	if (lastLoop && !g_loop)
	{
		xloop.reset();
		yloop.reset();
		return;
	}
#endif
	xerror = x-XCENTER;
	yerror = -(y-YCENTER);
	xloop.update(xerror);
	yloop.update(yerror);

	lastLoop = g_loop;
}
#endif

class MotorLoop
{
public:
	MotorLoop(uint32_t pgain, uint32_t dgain);

	int32_t update(int32_t error);

private:
	int32_t m_prevError;
	int32_t m_pgain;
	int32_t m_dgain;
};

MotorLoop::MotorLoop(uint32_t pgain, uint32_t dgain)
{
	m_pgain = pgain;
	m_dgain = dgain;
	m_prevError = 0x80000000;
}

#define MOTOR_MAX 500
#define MOTOR_MIN -500
int32_t MotorLoop::update(int32_t error)
{
	int32_t vel = 0;

	if (m_prevError!=0x80000000)
	{	
		vel = (error*m_pgain + (error - m_prevError)*m_dgain)/1000;
		if (vel>MOTOR_MAX) 
			vel = MOTOR_MAX; 
		else if (vel<MOTOR_MIN) 
			vel = MOTOR_MIN;
		//printf("%d %d %d\n", m_axis, m_pos, vel);
	}
	m_prevError = error;

	return vel;
}


#define MOTOR_DEADBAND  30
int32_t scaleMotor(int32_t val)
{
	int32_t pos = 0;

	if (val>0)
		pos = val + MOTOR_DEADBAND;
	else if (val<0)
		pos = val - MOTOR_DEADBAND;

	pos += SERVO_CENTER;
			
	if (pos>SERVO_MAX) 
		pos = SERVO_MAX; 
	else if (pos<SERVO_MIN) 
		pos = SERVO_MIN;

	return pos;
}

void axisMap(int32_t in[], int32_t out[])
{
 	out[0] = (in[0] - in[1])/2;
	out[1] = (in[0] + in[1])/2;
}

void motor(uint32_t x, uint32_t y)
{
	static MotorLoop rloop(1600, 4500);
	static MotorLoop tloop(4000, 4000);

	int32_t xerror, yerror, axesIn[2], axesOut[2];

	xerror = XCENTER-x;
	yerror = YTRACK-y;
	
	axesIn[0] = tloop.update(yerror);
	axesIn[1] = rloop.update(xerror);

	axisMap(axesIn, axesOut);

	rcs_setPos(0, scaleMotor(axesOut[0]));
	rcs_setPos(1, scaleMotor(axesOut[1]));
}


 							

#if 0
#define QMEMSIZE   0x800
uint32_t g_qmem[QMEMSIZE];
uint32_t g_numRls = 0;

int getRLSFrameCallback(const int32_t &responsInt, const uint32_t &numRls)
{
	g_numRls = numRls;
	return 0;
}

#if 0
void blobProcess(void)
{
	uint32_t result, i, j=0;
	uint32_t x, y, *memory = (uint32_t *)RLS_MEMORY;
	int16_t bdata[8];

   	// get first frame (primer)
	result = cc_getRLSFrame(memory, RLS_MEMORY_SIZE, LUT_MEMORY, &g_numRls);
	
		
	while(1)
	{
		if (!g_loop)
		{
		// service calls
#ifdef SERVO						   
			servo(XCENTER, YCENTER);
#else
	  		motor(XCENTER, YTRACK);
#endif		
			while(g_chirpUsb->service());
			handleButton();
		}
		else
		{		
			// copy
			if (g_numRls>QMEMSIZE)
				g_numRls = QMEMSIZE;
			for (i=0; i<g_numRls; i++)
				g_qmem[i] = memory[i];


			// kick off next frame
			cc_getRLSFrame(memory, RLS_MEMORY_SIZE, LUT_MEMORY, NULL, false);
			// process this one
			cc_getMaxBlob(g_qmem, g_numRls, bdata);
			if (bdata[0]>0)
			{
				x = bdata[0]+(bdata[1]-bdata[0])/2;
				y = bdata[2]+(bdata[3]-bdata[2])/2;
#ifdef SERVO
				servo(x, y);
#else
				motor(x, y);
#endif
			}
			else
#ifdef SERVO						   
				servo(XCENTER, YCENTER);
#else
	  			motor(XCENTER, YTRACK);
#endif		
#if 0
			if (j%10==0)
			{
				if (bdata[0]>0)
					printf("%d %d\n", x, y);
				else
					printf("**\n");
			}
			j++;   			
#endif
			// check for result
			while(!g_chirpM0->service());

			// service calls
			while(g_chirpUsb->service());
		}
	} 		
}

#else
#define WINDOW  10
#define THRESH  5

uint32_t detect(uint32_t x, uint32_t y)
{
	static uint16_t mem[WINDOW];
	static uint32_t flag = 0;
	int32_t i, temp;
	int32_t max;

	// shift memory down
	for (i=0; i<WINDOW-1; i++)	
		mem[i] = mem[i+1];

	mem[WINDOW-1] = y; 

	if (flag<WINDOW)
	{
		flag++;
		return 0;
	} 
	
	for (i=WINDOW-2, max=0; i>=0; i--)
	{
		temp = mem[i]-mem[WINDOW-1];
		if (temp>max)
			max = temp;			
	}
	//printf("m=%d\n", max);
	if (max>=THRESH)
	{
		flag = 0;  // flush data
		return 1;
	}
	else
		return 0;
}
#define YOFFS  20

void move(uint32_t mode)
{
	static int32_t x = SERVO_CENTER;
	static int32_t y = SERVO_CENTER+YOFFS;
	int32_t rx, ry;

	if (mode)
	{
		// center
		x = SERVO_CENTER;
		y = SERVO_CENTER+YOFFS;
		rcs_setPos(0, x);
		rcs_setPos(1, y);

		delayus(250000);
	}
	else
	{
		// move down
		rx = rand()%170 + 70;
		ry = rand()%50 + 20;
		if (rx&1)
			x += rx;
		else
			x -= rx;
		if ((ry&0x03)==0)
			y += ry;
		else
			y -= ry;
		if (y>SERVO_MAX || y<SERVO_MIN+270+YOFFS ||
		x>SERVO_MAX || x<SERVO_MIN)
		{
			move(1);
			return;
		}
		rcs_setPos(0, x);
		rcs_setPos(1, y);
		delayus(150000);
	}
}

void blobProcess(void)
{
	uint32_t result, i, j=0;
	uint32_t x, y, loseCount=0, *memory = (uint32_t *)RLS_MEMORY;
	int16_t bdata[8];

   	move(1);

   	// get first frame (primer)
	result = cc_getRLSFrame(memory, LUT_MEMORY, &g_numRls);
			
	while(1)
	{
		if (!g_loop)
		{
		// service calls
			while(g_chirpUsb->service());
			handleButton();
		}
		else
		{		
			// copy
			if (g_numRls>QMEMSIZE)
				g_numRls = QMEMSIZE;
			for (i=0; i<g_numRls; i++)
				g_qmem[i] = memory[i];


			// kick off next frame
			cc_getRLSFrame(memory, LUT_MEMORY, NULL, false);
			// process this one
			cc_getMaxBlob(g_qmem, g_numRls, bdata);
			if (bdata[0]>0)
			{
				loseCount = 0;
				x = bdata[0]+(bdata[1]-bdata[0])/2;
				y = bdata[2]+(bdata[3]-bdata[2])/2;
				//servo(x, y);
				//printf("%d %d\n", x, y);
				if (detect(x, y))
				{
					//printf("***\n");
					move(0);
				}
			}
			else
			{
				loseCount++;
				if (loseCount==10)
 					move(1);
			}
			// check for result
			while(!g_chirpM0->service());

			// service calls
			while(g_chirpUsb->service());
		}
	} 		
}
#endif
#endif

#define SERVO_SCALE 100
void servoMove(uint8_t axis0, int32_t start0, int32_t end0, int32_t speed0, uint8_t axis1, int32_t start1, int32_t end1)
{
	int32_t pos0, pos1, speed1;
	int32_t i, n;

	start0 *= SERVO_SCALE;	
	end0 *=	SERVO_SCALE;
	start1 *= SERVO_SCALE;	
	end1 *=	SERVO_SCALE;

	if (end0<start0)
		speed0 = -speed0;
		
	n = (end0-start0)/speed0;
	speed1 = (end1-start1)/n;
			 
	for (i=0, pos0=start0, pos1=start1; i<n; i++, pos0+=speed0, pos1+=speed1)
	{
		delayus(10000);
		rcs_setPos(axis0, pos0/SERVO_SCALE);
		rcs_setPos(axis1, pos1/SERVO_SCALE);
	}	
}

int32_t r(int32_t val, int32_t p)
{
	int32_t rn;
	
	rn = rand()%(2*p) - p + 100;
	
	val = val*rn/100;
	
	return val; 
}


uint32_t transmitCallback(uint16_t *data, uint32_t len)
{
	uint32_t i, j;

	for (i=0, j=0; i<10; i++, j+=2)
		data[i] = (j<<8) + j+1;

	return 10;
} 	


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
	exec_init(g_chirpUsb);
	cc_init(g_chirpUsb);
		
#if 1
	exec_addProg(&g_progVideo);
	exec_addProg(&g_progBlobs);
	exec_addProg(&g_progPt);
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

