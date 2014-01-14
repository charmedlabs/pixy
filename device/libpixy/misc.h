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
void delayms(uint32_t ms);
void setTimer(uint32_t *timer);
uint32_t getTimer(uint32_t timer);
void showError(uint8_t num, uint32_t color, const char *message);


#ifdef __cplusplus
}
#endif

#endif
   