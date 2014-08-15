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
#include "rcservo.h"


Program g_progBlobs =
{
	"blobs",
	"perform color blob analysis",
	blobsSetup, 
	blobsLoop
};



int blobsSetup()
{
	uint8_t c;

	// setup camera mode
	cam_setMode(CAM_MODE1);
 	
	// load lut if we've grabbed any frames lately
	if (g_rawFrame.m_pixels)
		cc_loadLut();

	// setup qqueue and M0
	g_qqueue->flush();
	exec_runM0(0);

	// flush serial receive queue
	while(ser_getSerial()->receive(&c, 1));

	return 0;
}

void handleRecv()
{
	uint8_t i, a;
	static uint8_t state=0, b=1;
	uint16_t s0, s1;
	Iserial *serial = ser_getSerial();

	for (i=0; i<10; i++)
	{
		switch(state)
		{	
		case 0: // look for sync
			if(serial->receive(&a, 1)<1)
				return;
			if (a==0xff && b==0x00)
				state = 1;
			b = a;
			break;

		case 1:	// read rest of data
			if (serial->receiveLen()>=4)
			{
				serial->receive((uint8_t *)&s0, 2);
				serial->receive((uint8_t *)&s1, 2);

				//cprintf("servo %d %d\n", s0, s1);
				rcs_setPos(0, s0);
				rcs_setPos(1, s1);

				state = 0;
			}
			break;

		default:
			state = 0;
			break;
		}
	}
}

int blobsLoop()
{
	BlobA *blobs;
	uint32_t numBlobs;

	// create blobs
	g_blobs->blobify();

	// handle received data immediately
	handleRecv();

	// send blobs
	g_blobs->getBlobs(&blobs, &numBlobs);
	cc_sendBlobs(g_chirpUsb, blobs, numBlobs);

	ser_getSerial()->update();

	cc_setLED();
	
	// deal with any latent received data until the next frame comes in
	while(!g_qqueue->queued())
		handleRecv();

	return 0;
}
