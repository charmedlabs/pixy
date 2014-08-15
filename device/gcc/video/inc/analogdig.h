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
