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

#ifndef _PROGPT_H
#define _PROGPT_H

#include "exec.h"

#define PAN_AXIS    0
#define TILT_AXIS   1
#define X_CENTER    (CAM_RES2_WIDTH/2)
#define Y_CENTER    (CAM_RES2_HEIGHT/2)

extern Program g_progPt;

int ptSetup();
int ptLoop();
void ptLoadParams();

class ServoLoop
{
public:
	ServoLoop(uint8_t axis, uint32_t pgain, uint32_t dgain);

	void update(int32_t error);
	void reset();
	void setGains(int32_t pgain, int32_t dgain);

private:
	int32_t m_pos;
	int32_t m_prevError;
	uint8_t m_axis;
	int32_t m_pgain;
	int32_t m_dgain;
};

#endif
