#include <debug.h>
#include <pixy_init.h>
#include <pixyvals.h>
#include <pixy_init.h>
#include <misc.h>
#include <string.h>
#include "camera.h"
#include "led.h"
#include "conncomp.h"
#include "param.h"
#include "rcservo.h"
#include "spi.h"
#include "spifi_rom_api.h"


#define RLS_MEMORY_SIZE     0x8000 // bytes
#define RLS_MEMORY          ((uint8_t *)SRAM0_LOC)
#define LUT_MEMORY_SIZE		0x10000 // bytes
#define LUT_MEMORY			((uint8_t *)SRAM0_LOC + SRAM0_SIZE-LUT_MEMORY_SIZE)  // +0x100 make room for prebuf and palette

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


 							
void interpolateBayer(uint32_t width, uint32_t x, uint32_t y, uint8_t *pixel, uint32_t &r, uint32_t &g, uint32_t &b)
{
    if (y&1)
    {
        if (x&1)
        {
            r = *pixel;
            g = *(pixel-1);
            b = *(pixel-width-1);
        }
        else
        {
            r = *(pixel-1);
            g = *pixel;
            b = *(pixel-width);
        }
    }
    else
    {
        if (x&1)
        {
            r = *(pixel-width);
            g = *pixel;
            b = *(pixel-1);
        }
        else
        {
            r = *(pixel-width-1);
            g = *(pixel-1);
            b = *pixel;
        }
    }
}
void getColor(uint8_t *r, uint8_t *g, uint8_t *b)
{
	uint32_t x, y, R, G, B, rsum=0, gsum=0, bsum=0;
	uint8_t *frame = (uint8_t *)SRAM0_LOC;

	for (y=94; y<104; y++)
	{
		for (x=154; x<164; x++)
		{
			interpolateBayer(320, x, y, frame+320*y+x, R, G, B);
		 	rsum += R;
			gsum += G;
			bsum += B;
		}
	}
	*r = rsum/100;
	*g = gsum/100;
	*b = bsum/100;	 	
}

void saturate(uint8_t *r, uint8_t *g, uint8_t *b)
{
	uint8_t max, min, bias;
	float m, fr, fg, fb;

   	// find min
	if (*r<*b)
		min = *r;
	else
		min = *b;
	if (*g<min)
		min = *g;

	// find reasonable bias to subtract out
	bias = min*3/4;
	*r -= bias;
	*g -= bias;
	*b -= bias;

	// find max
	if (*r>*b)
		max = *r;
	else
		max = *b;
	if (*g>max)
		max = *g;

	// saturate
	m = 255.0/max;
	fr = m**r;
	fg = m**g;
	fb = m**b;

	*r = (uint8_t)fr;
	*g = (uint8_t)fg;				  
	*b = (uint8_t)fb;
}
	
			
void handleButton()
{
	static uint32_t btPrev = 0;
	uint32_t bt; 
	static uint8_t r, g, b;

	bt = button();

	if (bt)
	{
		cam_getFrame((uint8_t *)SRAM0_LOC, SRAM0_SIZE, 0x21, 0, 0, 320, 200);
		getColor(&r, &g, &b);
		saturate(&r, &g, &b);
		led_setRGB(r, g, b);	 	
	}
	else if (btPrev)
	{
		led_setRGB(0, 0, 0);
		delayus(50000);
		led_setRGB(r, g, b);	 	
		delayus(50000);
		led_setRGB(0, 0, 0);
		delayus(50000);
		led_setRGB(r, g, b);	 	
		delayus(50000);
		led_setRGB(0, 0, 0);		
	}

	btPrev = bt;
}

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
	result = cc_getRLSFrame(memory, RLS_MEMORY_SIZE, LUT_MEMORY, &g_numRls);
			
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
			cc_getRLSFrame(memory, RLS_MEMORY_SIZE, LUT_MEMORY, NULL, false);
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

int main(void) 
 {	
 	pixyInit(SRAM3_LOC, &LR0[0], sizeof(LR0));
	cc_init(g_chirpUsb);
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
#if 1
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

