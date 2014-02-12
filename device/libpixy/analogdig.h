#ifndef _ANALOGDIG_H
#define _ANALOGDIG_H

#include "iserial.h"

class AnalogDig : public Iserial
{
public:
	AnalogDig();
	void setDirection(bool x);

	// Iserial methods
	virtual int open();
	virtual int close();
	//virtual int receive(uint8_t *buf, uint32_t len);
	virtual int update();

private:
	bool m_x; // true->x direction, false->y direction
	bool m_lastDetect;
};

void ad_init();

extern AnalogDig *g_ad;

#endif
