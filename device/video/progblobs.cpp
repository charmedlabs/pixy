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
