#ifndef _MISC_H
#define _MISC_H

#include <inttypes.h>

uint32_t adc_get(uint32_t channel);
uint32_t button();
void delayus(uint32_t us);
void setTimer(uint32_t *timer);
uint32_t getTimer(uint32_t timer);

#endif
   