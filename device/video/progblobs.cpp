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

#include "progblobs.h"
#include "pixy_init.h"
#include "camera.h"
#include "conncomp.h"
#include "serial.h"


Program g_progBlobs =
{
	"blobs",
	"perform color blob analysis",
	blobsSetup, 
	blobsLoop
};



int blobsSetup()
{
	// setup camera mode
	cam_setMode(CAM_MODE1);
 	
	// load lut if we've grabbed any frames lately
	if (g_rawFrame.m_pixels)
		cc_loadLut();

	// setup qqueue and M0
	g_qqueue->flush();
	exec_runM0(0);

	return 0;
}

int blobsLoop()
{
	BlobA *blobs;
	uint32_t numBlobs;

	// create blobs
	g_blobs->blobify();

	// send blobs
	g_blobs->getBlobs(&blobs, &numBlobs);
	cc_sendBlobs(g_chirpUsb, blobs, numBlobs);

	ser_getSerial()->update();

	cc_setLED();

	return 0;
}
