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

#include "progpt.h"
#include "pixy_init.h"
#include "camera.h"
#include "qqueue.h"
#include "blobs.h"
#include "spi.h"
#include "rcservo.h"
#include "cameravals.h"
#include "conncomp.h"

extern Qqueue *g_qqueue;
extern Blobs *g_blobs;

Program g_progPt =
{
	"pantilt",
	"perform pan/tilt tracking",
	ptSetup, 
	ptLoop
};

static ServoLoop g_panLoop(PAN_AXIS, 500, 800);
static ServoLoop g_tiltLoop(TILT_AXIS, 700, 900);


ServoLoop::ServoLoop(uint8_t axis, uint32_t pgain, uint32_t dgain)
{
	m_pos = RCS_CENTER_POS;
	m_axis = axis;
	m_pgain = pgain;
	m_dgain = dgain;
	m_prevError = 0x80000000;
}

void ServoLoop::update(int32_t error)
{
	int32_t vel;

	if (m_prevError!=0x80000000)
	{	
		vel = (error*m_pgain + (error - m_prevError)*m_dgain)/1000;
		m_pos += vel;
		if (m_pos>RCS_MAX_POS) 
			m_pos = RCS_MAX_POS; 
		else if (m_pos<RCS_MIN_POS) 
			m_pos = RCS_MIN_POS;

		rcs_setPos(m_axis, m_pos);
		//cprintf("%d %d %d\n", m_axis, m_pos, vel);
	}
	m_prevError = error;
}

void ServoLoop::reset()
{
	m_pos = RCS_CENTER_POS;
	rcs_setPos(m_axis, m_pos);
}


int ptSetup()
{
	// setup camera mode
	cam_setMode(CAM_MODE1);

	// extend range of servos
	rcs_setLimits(0, -200, 200);
	rcs_setLimits(1, -200, 200);

	// Increasing the PWM frequency makes the servos zippier. 
	// Pixy updates at 50 Hz, so a default servo update freq of 50 Hz
	// adds significant latency to the control loop--- increasing to 100 Hz decreases this.
	// Increasing to more than 130 Hz or so creates buzzing, prob not good for the servo.
	rcs_setFreq(100);
 	
	g_panLoop.reset();
	g_tiltLoop.reset();

	// load lut if we've grabbed any frames lately
	if (g_rawFrame.m_pixels)
		cc_loadLut();

	// setup qqueue and M0
	g_qqueue->flush();
	exec_runM0(0);

	return 0;
}

int ptLoop()
{
	int32_t panError, tiltError;
	uint16_t *blob, x, y;
	BlobA *blobs;
	uint32_t numBlobs;


	// create blobs
	g_blobs->blobify();

	blob = g_blobs->getMaxBlob();
	if (blob)
	{
		x = blob[1] + (blob[2] - blob[1])/2;
		y = blob[3] + (blob[4] - blob[3])/2;

		panError = X_CENTER-x;
		tiltError = y-Y_CENTER;

		g_panLoop.update(panError);
		g_tiltLoop.update(tiltError);
	}

	// send blobs
	g_blobs->getBlobs(&blobs, &numBlobs);
	cc_sendBlobs(g_chirpUsb, blobs, numBlobs);

	cc_setLED();
	
	return 0;
}
