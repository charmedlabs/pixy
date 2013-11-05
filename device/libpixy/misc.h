#ifndef _MISC_H
#define _MISC_H

#include <inttypes.h>

#ifdef __cplusplus
extern "C"
{
#endif

uint32_t adc_get(uint32_t channel);
uint32_t button(void);
void delayus(uint32_t us);
void setTimer(uint32_t *timer);
uint32_t getTimer(uint32_t timer);


#ifdef __cplusplus
}
#endif

#endif
   