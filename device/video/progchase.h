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

#ifndef _PROGCHASE_H
#define _PROGCHASE_H

#include "exec.h"

#define LEFT_AXIS       0
#define RIGHT_AXIS      1
#define Y_TRACK         130

#define X_CENTER        (CAM_RES2_WIDTH/2)
#define MOTOR_MAX       500
#define MOTOR_MIN       -500
#define MOTOR_DEADBAND  30


extern Program g_progChase;

int chaseSetup();
int chaseLoop();
void chaseLoadParams();

class MotorLoop
{
public:
	MotorLoop(uint32_t pgain, uint32_t dgain);

	int32_t update(int32_t error);
	void setGains(int32_t pgain, int32_t dgain);

private:

	int32_t m_prevError;
	int32_t m_pgain;
	int32_t m_dgain;
};

#endif
