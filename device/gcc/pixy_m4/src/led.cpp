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
#include "misc.h"
#include "led.h"

static float g_ledOnCurrent[3];
static float g_ledMaxBrightness;
static float g_ledMaxCurrent;
static float g_ledScale;
static uint16_t g_ledMaxPWM[3]; 
static uint8_t g_ledVal[3];

static const ProcModule g_module[] =
{
	{
	"led_set",
	(ProcPtr)(int32_t (*)(const uint32_t &))led_set, 
	{CRP_INT32, END}, 
	"Set RGB LED values"
	"@p color 24-bit value with red=bits 16-23, green=bits 8-15, blue=bits 0-7"
	"@r 0 if success, negative if error"
	},
	{
	"led_setRGB",
	(ProcPtr)led_setRGB, 
	{CRP_INT8, CRP_INT8, CRP_INT8, END}, 
	"Set RGB LED values"
	"@p red value ranging from 0-255"
	"@p green value ranging from 0-255"
	"@p blue value ranging from 0-255"
	"@r 0 if success, negative if error"
	},
	{
	"led_setMaxCurrent",
	(ProcPtr)led_setMaxCurrent, 
	{CRP_INT32, END}, 
	"Set maximum current each RGB LED, can be used to limit brightness"
	"@p current current value in microamps, ranging from 0 to 20000"
	"@r 0 if success, negative if error"
	},
	{
	"led_getMaxCurrent",
	(ProcPtr)led_getMaxCurrent, 
	{END}, 
	"Get maximum current each RGB LED, can be used to limit brightness"
	"@r max curent in microamperes"
	},
	END
};	


void led_init()
{
	// turn on LEDs (max)
	led_setPWM(LED_RED, LED_MAX_PWM);
	led_setPWM(LED_GREEN, LED_MAX_PWM);
	led_setPWM(LED_BLUE, LED_MAX_PWM);

	// wait for things to settle...
	delayus(20000);

	// get current of each led.  This is needed because each LED has a different forward voltage.  But current determines
	// brightness regardless of voltage drop.  So we normalize with respect to current for best color accuracy. 
	g_ledOnCurrent[LED_RED] = (float)adc_get(LED_RED_ADCCHAN)/ADC_MAX*ADC_VOLTAGE/LED_RED_RESISTOR;
	g_ledOnCurrent[LED_GREEN] = (float)adc_get(LED_GREEN_ADCCHAN)/ADC_MAX*ADC_VOLTAGE/LED_GREEN_RESISTOR;
	g_ledOnCurrent[LED_BLUE] = (float)adc_get(LED_BLUE_ADCCHAN)/ADC_MAX*ADC_VOLTAGE/LED_BLUE_RESISTOR;	

	g_ledVal[LED_RED] = 0xff;
	g_ledVal[LED_GREEN] = 0xff;
	g_ledVal[LED_BLUE] = 0xff;

	// turn off LEDs
	led_set(0);
		
	// set other vals...
	g_ledScale = LED_DEFAULT_SCALE;
	led_setMaxCurrent(LED_DEFAULT_MAX_CURRENT);	
	
	g_chirpUsb->registerModule(g_module);
}

void led_setPWM(uint8_t led, uint16_t pwm)
{
	if (led>2)
		return;

#if 0
	// this code causes some glitchy looking flashes because it messes with 
	// the pwm state machine, although this isn't clear to me why it would 
	// be slow enough to be visible....
	// This code turns the LED completely off --- disabling for now though.  
	if (pwm==0)
	{
	 	LPC_SCT->OUT[led].SET = 1<<15; // disable
		LPC_SCT->OUT[led].CLR = 1<<3;
	} 
	else
#endif 
	if (pwm==LED_MAX_PWM)
	{
		LPC_SCT->OUT[led].SET = 1<<3;
	 	LPC_SCT->OUT[led].CLR = 1<<15; 
	}
	else
	{
		LPC_SCT->MATCHREL[led+1].H = pwm;
		LPC_SCT->MATCH[led+1].H = pwm;
		LPC_SCT->OUT[led].SET = 1<<3;
		LPC_SCT->OUT[led].CLR = 1<<(led+4);
	}	
}

void led_set(uint8_t led, uint8_t val, bool override)
{
	float brightness, current, pwm;

	if (led>2)
		return;

	if (!override && g_ledVal[led]==val)
		return;
				
	brightness = val/255.0*g_ledMaxBrightness;

	// invert brightness to get current
	if (val==0)
		current = 0;
	else
		current = exp(brightness)/g_ledScale;

	// convert current into pwm
	pwm = current/g_ledMaxCurrent*g_ledMaxPWM[led];
 
	if ((uint32_t)pwm>LED_MAX_PWM)
		led_setPWM(led, LED_MAX_PWM);  // this shouldn't really happen, but might because of rounding
	else
		led_setPWM(led, (uint16_t)pwm);

	g_ledVal[led] = val;
}


int32_t led_setRGB(const uint8_t &r, const uint8_t &g, const uint8_t &b)
{
	led_set(LED_RED, r);
	led_set(LED_GREEN, g);
	led_set(LED_BLUE, b);

	return 0;
}

int32_t led_set(const uint32_t &color)
{
	uint8_t r, g, b;

	r = (color>>16)&0xff;
	g = (color>>8)&0xff;
	b = color&0xff;
	led_set(LED_RED, r);
	led_set(LED_GREEN, g);
	led_set(LED_BLUE, b);

	return 0;
}


int32_t led_setMaxCurrent(const uint32_t &uamps)
{
	int i;
	float pwm;

	// convert current back to amps
	g_ledMaxCurrent = (float)uamps/1000000;

	// set maxCurrent to minimum of max (so we saturate only 1 led)
	for (i=0; i<3; i++)
	{
		if (g_ledMaxCurrent>g_ledOnCurrent[i])
			g_ledMaxCurrent = g_ledOnCurrent[i];
	}

	// brightness is log of current * constant
	g_ledMaxBrightness = log(g_ledScale*g_ledMaxCurrent);

	for (i=0; i<3; i++)
	{
		pwm = g_ledMaxCurrent/g_ledOnCurrent[i]*LED_MAX_PWM;
		if ((uint32_t)pwm>LED_MAX_PWM)
			g_ledMaxPWM[i] = LED_MAX_PWM;  // this shouldn't really happen, but might because of rounding
		else
			g_ledMaxPWM[i] = (uint16_t)pwm;

		// restore vals (with new max current)
	   led_set(i, g_ledVal[i], true);
	}

	return 0;
}


uint32_t led_getMaxCurrent()
{
	return g_ledMaxCurrent*1000000;
}
