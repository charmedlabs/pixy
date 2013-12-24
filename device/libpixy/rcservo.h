#ifndef _RCSERVO_H
#define _RC_SERVO_H

#define RCS_CLOCK_FREQ	1000000
#define RCS_MAX_POS     1000
#define RCS_MIN_PWM     1000
#define RCS_MAX_PWM     2000
#define RCS_PWM_RANGE   (RCS_MAX_PWM-RCS_MIN_PWM)

#define RCS_GAIN_SCALE  10
#define RCS_NUM_AXES    2

void rcs_init();

int32_t rcs_setPos(const uint8_t &channel, const uint16_t &pos);
int32_t rcs_getPos(const uint8_t &channel);
int32_t rcs_enable(const uint8_t &channel, const uint8_t &enable);
int32_t rcs_setLimits(const uint8_t &channel, const int16_t &lower, const int16_t &upper);
int32_t rcs_setFreq(const uint16_t &freq);

#endif

												   