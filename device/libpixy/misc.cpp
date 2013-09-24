#include "misc.h"
#include "lpc43xx_adc.h"

// can be called before timer is set up
void delayus(uint32_t us)
{
	volatile uint32_t i, j;	
	
	for (i=0; i<us; i++)
		for (j=0; j<38; j++);
}

uint32_t button()
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
