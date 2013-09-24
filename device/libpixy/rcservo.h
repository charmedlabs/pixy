#ifndef _RCSERVO_H
#define _RC_SERVO_H

#define RCS_MAX_POS     1000
#define RCS_MIN_PWM     1000
#define RCS_MAX_PWM     2000

void rcs_init();

int32_t rcs_setPos(const uint8_t &channel, const uint16_t &pos, Chirp *chirp=NULL);
int32_t rcs_getPos(const uint8_t &channel, Chirp *chirp=NULL);
int32_t rcs_enable(const uint8_t &channel, const uint8_t &enable, Chirp *chirp=NULL);

#endif

												   