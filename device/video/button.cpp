#include "pixy_init.h"
#include "button.h"
#include "camera.h"
#include "blobs.h"
#include "led.h"
#include "misc.h"
#include "colorlut.h"

void getColor(uint8_t *r, uint8_t *g, uint8_t *b)
{
	uint32_t x, y, R, G, B, rsum=0, gsum=0, bsum=0;
	uint8_t *frame = (uint8_t *)SRAM1_LOC;

	for (y=94; y<104; y++)
	{
		for (x=154; x<164; x++)
		{
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
	

static const uint32_t g_colors[] = 
{
	0xffffff, // 0 white
	0xff0000, // 1 red
	0xffa000, // 2 orange
	0xffff00, // 3 yellow
	0x00ff00, // 4 green
	0x00ffff, // 5 cyan
	0x0000ff, // 6 blue
	0xff00ff  // 7 violet
};

ButtonMachine::ButtonMachine()
{
	reset();
}

ButtonMachine::~ButtonMachine()
{
}

#define TIMEOUT1    1250*1000
#define TIMEOUT2    1000*1000
#define TIMEOUT3    15000*1000
#define TIMEOUT_FLASH 75*1000

int ButtonMachine::handleSignature()
{
	uint32_t bt; 

	bt = button();

   	if (m_ledPipe)
		led_set(0xff);

	switch(m_goto)
	{
	case 0:  // wait for button press
		if (bt)
		{
			setTimer(&m_timer);
			m_goto = 1;
		}
		break;

	case 1: // wait for button timeout
		if (!bt)
			m_goto = 0;
		else if (getTimer(m_timer)>TIMEOUT1)
		{
			m_index = 0;
			setTimer(&m_timer);
			setLED();
			m_goto = 2;
		}
		break;

	case 2: // wait and increment index 
		if (!bt)
		{
			flashLED(3);
			setTimer(&m_timer);
			if (m_index==0)
				cam_setAWB(1);
			else
				m_ledPipe = true;
			m_goto = 3;
		}
		else if (getTimer(m_timer)>TIMEOUT2)
		{
			setTimer(&m_timer);
			m_index++;
			if (m_index==NUM_MODELS+1)
				m_index = 0;

			setLED();
		}							   
		break;

	case 3: // wait for button down
		if (bt)
		{
			setTimer(&m_timer);
			m_goto = 4;
		}
		else if (getTimer(m_timer)>TIMEOUT3) // abort
			reset();
		break;

	case 4: // wait for button up
		if (!bt)
		{
			if (m_index==0)
			{
				cam_setAWB(0);
				flashLED(4); // todo: flash according to saturation
			}
			else
			{
				// grow region, create model, save
				flashLED(4); // todo: flash according to saturation
			}
			reset(); // done	
		}
		else if (getTimer(m_timer)>TIMEOUT1)
			reset();
	 	break;

	default:
		reset();
	}	

	return m_ledPipe;
}

void ButtonMachine::wait(uint32_t us)
{
	uint32_t timer;

	setTimer(&timer);

	while(getTimer(timer)<us)
		periodic();
}

void ButtonMachine::reset()
{
	m_ledPipe = false;
	led_set(0);
	m_goto = 0;
}

void ButtonMachine::flashLED(uint8_t flashes)
{
	 int i;

	 for (i=0; i<flashes; i++)
	 {
		led_set(0);
		wait(TIMEOUT_FLASH); // flash for just a little bit
		led_set(g_colors[m_index]);
		wait(TIMEOUT_FLASH); // flash for just a little bit
	 }
	 	
}


void ButtonMachine::setLED()
{
	if (m_index>7)
		return;

	led_set(0);
	wait(TIMEOUT_FLASH); // flash for just a little bit
	led_set(g_colors[m_index]);
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

