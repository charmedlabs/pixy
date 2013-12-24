#include "progpt.h"
#include "progblobs.h"
#include "pixy_init.h"
#include "camera.h"
#include "qqueue.h"
#include "blobs.h"
#include "spi.h"
#include "rcservo.h"

extern Qqueue *g_qqueue;
extern Blobs *g_blobs;

Program g_progPt =
{
	"pantilt",
	"perform pan/tilt tracking",
	ptSetup, 
	ptLoop
};

int ptSetup()
{
	// setup camera mode
	cam_setMode(CAM_MODE1);

	// extend range of servos
	rcs_setLimits(0, -200, 200);
	rcs_setLimits(1, -200, 200);
	// increasing the PWM frequency makes the servos zippier 
	// increasing to more than 130 or so creates buzzing
	rcs_setFreq(100);

	// setup spi
	spi_setCallback(spiCallback);
 	
	// setup qqueue and M0
	g_qqueue->flush();
	exec_runM0(0);

	return 0;
}

int ptLoop()
{
	uint16_t recvBuf[1];

	// create blobs
	g_blobs->blobify();

	// just grab a word-- don't worry about looking at data except to see if we're synchronized.
	if (spi_receive(recvBuf, 1))
	{
		if (recvBuf[0]!=0xa5a5) // if received data isn't correct, we're out of sync
			spi_sync();
		//cprintf("%x\n", recvBuf[0]); 
	}
	
	return 0;
}
