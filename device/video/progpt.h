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

class ServoLoop
{
public:
	ServoLoop(uint8_t axis, uint32_t pgain, uint32_t dgain);

	void update(int32_t error);
	void reset();

private:
	int32_t m_pos;
	int32_t m_prevError;
	uint8_t m_axis;
	int32_t m_pgain;
	int32_t m_dgain;
};

#endif
