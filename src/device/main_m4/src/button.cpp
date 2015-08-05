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

#include <math.h>
#include "pixy_init.h"
#include "button.h"
#include "camera.h"
#include "blobs.h"
#include "led.h"
#include "misc.h"
#include "colorlut.h"
#include "conncomp.h"
#include "exec.h"
#include "calc.h"

#define BT_CENTER_SIZE   6


#define SA_GAIN   0.015f
#define G_GAIN 1.10f

// this routine needs to provide good feedback to the user as to whether the camera sees and segments the object correctly.
// The key is to integrate the "growing algorithm" such that the growing algorithm is executed continuously and feedback about
// the "goodness" of the grown region is returned.  We're choosing goodness to be some combination of size (because the bigger 
// the grown region the better) and saturation (because more saturation the more likely we've found the intended target, vs 
// the background, which is typically not saturated.)  
// In general with an RGB LED, you can only communicate 2 things--- brightness and hue, so you have 2 dof to play with....   
void scaleLED(uint32_t r, uint32_t g, uint32_t b, uint32_t n)
{
	uint32_t max, min, current, sat, t; 

#if 0  // it seems that green is a little attenuated on this sensor
	t = (uint32_t)(G_GAIN*g);
	if (t>255)
		g = 255;
	else
		g = t;
#endif

   	// find min
	min = MIN(r, g);
	min = MIN(min, b);

	// find max
	max = MAX(r, g);
	max = MAX(max, b);

	// subtract min and form sataration from the distance from origin
	sat = sqrt((float)((r-min)*(r-min) + (g-min)*(g-min) + (b-min)*(b-min)));
	if (sat>30) // limit saturation to preven things from getting too bright
		sat = 30;
	if (sat<10) // anything less than 15 is pretty uninteresting, no sense in displaying....
		current = 0;
	else
	{
		//sat2 = exp(sat/13.0f);
		//current = (uint32_t)(SAT_GAIN*sat2) + (uint32_t)(AREA_GAIN*n) + (uint32_t)(SA_GAIN*n*sat2);
		current = (uint32_t)(SA_GAIN*n*sat);
	}
	if (current>LED_MAX_CURRENT/5)
		current = LED_MAX_CURRENT/5;
	led_setMaxCurrent(current);

#if 0
	// find reasonable bias to subtract out
	bias = min*75/100;
	r -= bias;
	g -= bias;
	b -= bias;
	
	// saturate
	m = 255.0f/(max-bias);
	r = (uint8_t)(m*r);
	g = (uint8_t)(m*g);
	b = (uint8_t)(m*b);
#endif
#if 1
	// saturate
	rgbUnpack(saturate(rgbPack(r, g, b)), &r, &g, &b);
#endif
	//cprintf("r %d g %d b %d min %d max %d sat %d sat2 %d n %d\n", r, g, b, min, max, sat, sat2, n);
	led_setRGB(r, g, b);	 	
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
	Points points;
	RGBPixel rgb;
	uint32_t color, r, g, b, n;
	g_blobs->m_clut.growRegion(g_rawFrame, Point16(CAM_RES2_WIDTH/2, CAM_RES2_HEIGHT/2), &points);	
	cc_sendPoints(points, CL_GROW_INC, CL_GROW_INC, g_chirpUsb);

	IterPixel ip(g_rawFrame, &points);
	color = ip.averageRgb(&n);

	rgbUnpack(color, &r, &g, &b);
	scaleLED(r, g, b, n);
}

void ButtonMachine::setSignature()
{
	int res;

	// grow region, create model, save
	res = cc_setSigPoint(0, m_index, CAM_RES2_WIDTH/2, CAM_RES2_HEIGHT/2);
	if (res<0)
		return;
	exec_sendEvent(g_chirpUsb, EVT_PARAM_CHANGE);
	flashLED(4); 
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
			led_setMaxCurrent(g_ledBrightness); // restore default brightness
			m_goto = 1;
			led_set(0);
		}
		break;

	case 1: // wait for button timeout
		if (!bt)
			m_goto = 0;
		else if (getTimer(m_timer)>BT_INITIAL_BUTTON_TIMEOUT)
		{
			if (cam_getAWB())
				m_index = 1;
			else
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
			if (m_index==CL_NUM_SIGNATURES+1)
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

bool ButtonMachine::selectProgram(uint8_t progs, uint8_t *selectedProg)
{
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
				m_index=0;
				setLED();
				m_goto = 2;
			}
			else if (getTimer(m_timer)>BT_PROG_TIMEOUT)
			{
				m_index = *selectedProg;
				flashLED(4); 
				reset();
				return false;
			}
			break;

		case 2: // cycle through choices, wait for button up
			if (!bt)
			{
				*selectedProg = m_index; // save m_index
				flashLED(4); 
				reset(); // resets m_index
				return true;
			}
			else if (getTimer(m_timer)>BT_INDEX_CYCLE_TIMEOUT)
			{
				setTimer(&m_timer);
				m_index++;
				if (m_index==progs)
					m_index = 0;

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
	m_index = 0;
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

			
