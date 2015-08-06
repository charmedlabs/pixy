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

#include "progchase.h"
#include "pixy_init.h"
#include "camera.h"
#include "qqueue.h"
#include "blobs.h"
#include "spi.h"
#include "rcservo.h"
#include "cameravals.h"
#include "conncomp.h"
#include "param.h"

extern Qqueue *g_qqueue;
extern Blobs *g_blobs;

Program g_progChase =
{
	"chase",
	"chase an object (demo)",
	chaseSetup, 
	chaseLoop
};

static MotorLoop g_transLoop(500, 800);
static MotorLoop g_rotLoop(700, 900);
static int32_t scaleMotor(int32_t val);
static void axisMap(int32_t in[], int32_t out[]);

MotorLoop::MotorLoop(uint32_t pgain, uint32_t dgain)
{
	m_pgain = pgain;
	m_dgain = dgain;
	m_prevError = 0x80000000; // to indicate that it's never been set
}

// control loop update!
int32_t MotorLoop::update(int32_t error)
{
	int32_t vel;

	if (m_prevError!=0x80000000)
	{	
		vel = (error*m_pgain + (error - m_prevError)*m_dgain)/1000;	// calc proportional-derivative 
		// saturation
		if (vel>MOTOR_MAX) 
			vel = MOTOR_MAX; 
		else if (vel<MOTOR_MIN) 
			vel = MOTOR_MIN;
	}
	m_prevError = error;

	return vel;
}


void MotorLoop::setGains(int32_t pgain, int32_t dgain)
{
	m_pgain = pgain;
	m_dgain = dgain;	
}

int32_t scaleMotor(int32_t val)
{
	int32_t pos = 0;

	if (val>0)
		pos = val + MOTOR_DEADBAND;
	else if (val<0)
		pos = val - MOTOR_DEADBAND;

	pos += RCS_CENTER_POS;
			
	if (pos>RCS_MAX_POS) 
		pos = RCS_MAX_POS; 
	else if (pos<RCS_MIN_POS) 
		pos = RCS_MIN_POS;

	return pos;
}

// Map axes!  go from translational/rotational to left/right 
void axisMap(int32_t in[], int32_t out[])
{
 	out[0] = (in[0] - in[1])/2;
	out[1] = (in[0] + in[1])/2;
}

// calculate left and right wheel commands
void combine(uint32_t x, uint32_t y)
{
	int32_t xError, yError, axesIn[2], axesOut[2];

	xError = X_CENTER-x;
	yError = Y_TRACK-y;
	
	//cprintf("x: %d y: %d xError: %d yError: %d\n", x, y, xError, yError);

	axesIn[0] = g_transLoop.update(yError);
	axesIn[1] = g_rotLoop.update(xError);

	axisMap(axesIn, axesOut);

	rcs_setPos(LEFT_AXIS, scaleMotor(axesOut[0]));
	rcs_setPos(RIGHT_AXIS, scaleMotor(axesOut[1]));
}	

int chaseSetup()
{
	// setup camera mode
	cam_setMode(CAM_MODE1);

 	chaseLoadParams();
	rcs_setPos(LEFT_AXIS, RCS_CENTER_POS);
	rcs_setPos(RIGHT_AXIS, RCS_CENTER_POS);
	
	// load lut if we've grabbed any frames lately
	if (g_rawFrame.m_pixels)
		cc_loadLut();

	// setup qqueue and M0
	g_qqueue->flush();
	exec_runM0(0);

	return 0;
}

void chaseLoadParams()
{
	prm_add("Translation P gain", 0, 
		"@c Chase_demo tranlational proportional gain (default 500)", INT32(500), END);
	prm_add("Translation D gain", 0, 
		"@c Chase_demo translational derivative gain (default 800)", INT32(800), END);
	prm_add("Rotation P gain", 0, 
		"@c Chase_demo rotational proportional gain (default 500)", INT32(500), END);
	prm_add("Rotation D gain", 0, 
		"@c Chase_demo rotational derivative gain (default 800)", INT32(800), END);

	int32_t pgain, dgain; 

	prm_get("Translation P gain", &pgain, END);
	prm_get("Translation D gain", &dgain, END);
	g_transLoop.setGains(pgain, dgain);
	prm_get("Rotation P gain", &pgain, END);
	prm_get("Rotation D gain", &dgain, END);
	g_rotLoop.setGains(pgain, dgain);
}


int chaseLoop()
{
	uint16_t x, y;
	BlobA *blobs, *blob;
	BlobB *ccBlobs;
	uint32_t numBlobs, numCCBlobs;


	// create blobs
	g_blobs->blobify();

	blob = (BlobA *)g_blobs->getMaxBlob();
	if (blob)
	{
		x = blob->m_left + (blob->m_right - blob->m_left)/2;
		y = blob->m_top + (blob->m_bottom - blob->m_top)/2;

		combine(x, y);
	}
	else
	{
		rcs_setPos(LEFT_AXIS, RCS_CENTER_POS);
		rcs_setPos(RIGHT_AXIS, RCS_CENTER_POS);
	}


	// send blobs
	g_blobs->getBlobs(&blobs, &numBlobs, &ccBlobs, &numCCBlobs);
	cc_sendBlobs(g_chirpUsb, blobs, numBlobs, ccBlobs, numCCBlobs);

	cc_setLED();
	
	return 0;
}
