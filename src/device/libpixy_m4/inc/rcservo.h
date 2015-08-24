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

#ifndef _RCSERVO_H
#define _RC_SERVO_H

#define RCS_MIN_POS     0
#define RCS_MAX_POS     1000
#define RCS_CENTER_POS	((RCS_MAX_POS-RCS_MIN_POS)/2)

#define RCS_MIN_PWM     1000
#define RCS_MAX_PWM     2000
#define RCS_PWM_RANGE   (RCS_MAX_PWM-RCS_MIN_PWM)

#define RCS_CLOCK_FREQ	1000000

#define RCS_GAIN_SCALE  10
#define RCS_NUM_AXES    2

void rcs_init();
void rcs_loadParams();

int32_t rcs_setPos(const uint8_t &channel, const uint16_t &pos);
int32_t rcs_getPos(const uint8_t &channel);
int32_t rcs_enable(const uint8_t &channel, const uint8_t &enable);
int32_t rcs_setLimits(const uint8_t &channel, const int16_t &lower, const int16_t &upper);
int32_t rcs_setFreq(const uint16_t &freq);

#endif

