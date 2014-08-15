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

#include "pixy_init.h"
#include "button.h"
#include "camera.h"
#include "blobs.h"
#include "led.h"
#include "misc.h"
#include "colorlut.h"
#include "conncomp.h"

#define BT_CENTER_SIZE   6

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
	uint32_t x, y, R, G, B, rsum, gsum, bsum, count;
	uint8_t *frame = g_rawFrame.m_pixels;  // use the correct pointer

	for (rsum=0, gsum=0, bsum=0, count=0, y=(CAM_RES2_HEIGHT-BT_CENTER_SIZE)/2; y<(CAM_RES2_HEIGHT+BT_CENTER_SIZE)/2; y++)
	{
		for (x=(CAM_RES2_WIDTH-BT_CENTER_SIZE)/2; x<(CAM_RES2_WIDTH+BT_CENTER_SIZE)/2; x++, count++)
		{
			interpolateBayer(CAM_RES2_WIDTH, x, y, frame+CAM_RES2_WIDTH*y+x, R, G, B);
		 	rsum += R;
			gsum += G;
			bsum += B;
		}
	}
	*r = rsum/count;
	*g = gsum/count;										 
	*b = bsum/count;	 	
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
	

ButtonMachine::ButtonMachine()
{
	reset();
}

ButtonMachine::~ButtonMachine()
{
}

void ButtonMachine::ledPipe()
{
	uint8_t r, g, b;

	BlobA blob(m_index, (CAM_RES2_WIDTH-BT_CENTER_SIZE)/2, (CAM_RES2_WIDTH+BT_CENTER_SIZE)/2, (CAM_RES2_HEIGHT-BT_CENTER_SIZE)/2, (CAM_RES2_HEIGHT+BT_CENTER_SIZE)/2);
	cc_sendBlobs(g_chirpUsb, &blob, 1);

	getColor(&r, &g, &b);
	saturate(&r, &g, &b);
	led_setRGB(r, g, b);	 	
}

void ButtonMachine::setSignature()
{
	uint32_t current, saveCurrent; 
	int goodness;

	// grow region, create model, save
	goodness = cc_setSigPoint(m_index, CAM_RES2_WIDTH/2, CAM_RES2_HEIGHT/2, g_chirpUsb);
	if (goodness>0)
	{
		cprintf("goodness=%d\n", goodness);
		saveCurrent = led_getMaxCurrent(); // save off value
		current = (float)LED_MAX_CURRENT/100.0f*goodness;
		led_setMaxCurrent(current);
		flashLED(4); 
		led_setMaxCurrent(saveCurrent);
	}
}

bool ButtonMachine::handleSignature()
{
	uint32_t bt;

	bt = button();

   	if (m_ledPipe) // if ledpipe, grab frame, but don't flush 
	{
		cam_getFrameChirpFlags(CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT, g_chirpUsb, 0);
		ledPipe();
	}
	else if (m_goto!=0) // else grab frame and flush
		cam_getFrameChirpFlags(CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT, g_chirpUsb);

	switch(m_goto)
	{
	case 0:  // wait for button press
		if (bt)
		{
			setTimer(&m_timer);
			m_goto = 1;
			led_set(0);
		}
		break;

	case 1: // wait for button timeout
		if (!bt)
			m_goto = 0;
		else if (getTimer(m_timer)>BT_INITIAL_BUTTON_TIMEOUT)
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
		else if (getTimer(m_timer)>BT_INDEX_CYCLE_TIMEOUT)
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
		else if (getTimer(m_timer)>BT_LIGHTPIPE_TIMEOUT) // abort
			reset();
		break;

	case 4: // wait for button up
		if (!bt)
		{
			if (m_index==0)
			{
				cam_setAWB(0);
				flashLED(4); 
			}
			else
				setSignature();
			reset(); // done	
		}
		else if (getTimer(m_timer)>BT_INITIAL_BUTTON_TIMEOUT)
		{
 			if (m_index==0)
				cam_setAWB(0);

			reset();
			m_goto = 5;
		}
	 	break;

	case 5: // wait for button up only
		if (!bt)
			reset();
		break;

	default:
		reset();
	}	

	return m_goto!=0;
}

int ButtonMachine::selectProgram(int progs)
{
	int result;

	uint32_t bt; 

	if (progs<=1)
		return 0;

	while(1)
	{
		bt = button();
		periodic();

		switch(m_goto)
		{
		case 0:  // wait for nothing
			setTimer(&m_timer);
			m_goto = 1;
			setLED();
			break;

		case 1:	// wait for button down
			if (bt)
			{
				setTimer(&m_timer);
				m_index=1;
				setLED();
				m_goto = 2;
			}
			else if (getTimer(m_timer)>BT_PROG_TIMEOUT)
			{
				flashLED(4); 
				reset();
				return 0;
			}
			break;

		case 2: // cycle through choices, wait for button up
			if (!bt)
			{
				result = m_index; // save m_index
				flashLED(4); 
				reset(); // resets m_index
				return result;
			}
			else if (getTimer(m_timer)>BT_INDEX_CYCLE_TIMEOUT)
			{
				setTimer(&m_timer);
				m_index++;
				if (m_index==progs+1)
					m_index = 1;

				setLED();
			}							   
			break;

		default:
			reset();
		}
	}
}



void ButtonMachine::wait(uint32_t us)
{
	uint32_t timer;

	setTimer(&timer);

	while(getTimer(timer)<us);
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
		wait(BT_FLASH_TIMEOUT); // flash for just a little bit
		led_set(g_colors[m_index]);
		wait(BT_FLASH_TIMEOUT); // flash for just a little bit
	 }
	 	
}


void ButtonMachine::setLED()
{
	if (m_index>7)
		return;

	led_set(0);
	wait(BT_FLASH_TIMEOUT); // flash for just a little bit
	led_set(g_colors[m_index]);
}

			
