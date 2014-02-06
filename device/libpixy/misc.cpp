#include "misc.h"
#include "lpc43xx_adc.h"
#include "pixy_init.h"
#include "debug.h"
#include "led.h"

// can be called before timer is set up
void delayus(uint32_t us)
{
	uint32_t timer;
	
	setTimer(&timer);
	
	while(getTimer(timer)<us);
}


void delayms(uint32_t ms)
{
	uint32_t timer;
	
	setTimer(&timer);

	ms *= 1000;
	
	while(getTimer(timer)<ms);
}


uint32_t button(void)
{
	static uint32_t bPrev = 0;
	uint32_t b;

	b = LPC_GPIO_PORT->PIN[5] & (1<<7);

	// debounce	
	if (b!=bPrev)
	{
		delayus(2000);
		b = LPC_GPIO_PORT->PIN[5] & (1<<7);
	}
	
	bPrev = b;		 
	return b;
}

uint32_t adc_get(uint32_t channel)
{
	uint32_t res;

	ADC_ChannelCmd(LPC_ADC0, channel, ENABLE);
	delayus(500);
	ADC_StartCmd(LPC_ADC0, ADC_START_NOW);
	while (!(ADC_ChannelGetStatus(LPC_ADC0, channel, ADC_DATA_DONE)));
	res = ADC_ChannelGetData(LPC_ADC0, channel);
	ADC_ChannelCmd(LPC_ADC0, channel, DISABLE);

	return res;
}

void setTimer(uint32_t *timer)
{
	*timer = LPC_TIMER2->TC;
}

uint32_t getTimer(uint32_t timer)
{
	uint32_t result; 
	result = LPC_TIMER2->TC-timer;	

	return result;
}

void showError(uint8_t num, uint32_t color, const char *message)
{
	int i;

	while(1)
	{
		// flash signal number
		for (i=0; i<num; i++)
		{
			led_set(color);
			delayus(150000);
			led_setRGB(0, 0, 0);
			delayus(150000);
		}
		delayus(500000);
		// print message
		if (message)
			lpc_printf(message);
	}
 }
