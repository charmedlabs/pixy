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

#ifndef _LED_H
#define _LED_H

#include "chirp.hpp"

#define LED_RED                   0
#define LED_GREEN                 1
#define LED_BLUE                  2	 
//#define LED_RED_ADCCHAN           0
#define LED_RED_ADCCHAN           6
#define LED_GREEN_ADCCHAN         1
#define LED_BLUE_ADCCHAN          2
#define LED_RED_RESISTOR          150
#define LED_GREEN_RESISTOR        91
#define LED_BLUE_RESISTOR         91

#define LED_MAX_PWM               0xffff
#define ADC_MAX                   0x3ff
#define ADC_VOLTAGE               3.3f

// the 2 values below might be correlated unintentionally
// The larger the default scale, the more attenuation of the brightness on the low end
// but note, the range is huge.  For example, there isn't much difference between 20000 and 60000
#define LED_DEFAULT_SCALE         100000.0
#define LED_DEFAULT_MAX_CURRENT   750  // uA, can be as high as 20000, but it's difficult to look at (too bright!)
#define LED_MAX_CURRENT           20000 

void led_init();
void led_setPWM(uint8_t led, uint16_t pwm);
void led_set(uint8_t led, uint8_t val, bool override=false);
int32_t led_set(const uint32_t &color);
int32_t led_setRGB(const uint8_t &r, const uint8_t &g, const uint8_t &b);
int32_t led_setMaxCurrent(const uint32_t &uamps);
uint32_t led_getMaxCurrent();


#endif
