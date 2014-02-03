#include "progblobs.h"
#include "pixy_init.h"
#include "camera.h"
#include "qqueue.h"
#include "blobs.h"
#include "spi.h"
#include "conncomp.h"

extern Qqueue *g_qqueue;
extern Blobs *g_blobs;

Program g_progBlobs =
{
	"blobs",
	"perform color blob analysis",
	blobsSetup, 
	blobsLoop
};

uint32_t spiCallback(uint16_t *data, uint32_t len)
{
	if (len>=8)
		return g_blobs->getBlock(data);

	return 0;
} 	


int blobsSetup()
{
	// setup camera mode
	cam_setMode(CAM_MODE1);

	// setup spi
	spi_setCallback(spiCallback);
 	
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
	uint16_t recvBuf[1];
	BlobA *blobs;
	uint32_t numBlobs;
	static uint8_t syncCounter = 0;

	// create blobs
	g_blobs->blobify();

	// send blobs
	g_blobs->getBlobs(&blobs, &numBlobs);
	cc_sendBlobs(g_chirpUsb, blobs, numBlobs);

	// just grab a word-- don't worry about looking at data except to see if we're synchronized.
	if (spi_receive(recvBuf, 1))
	{
		if ((recvBuf[0]&0xff00)!=0x5a00) // if received data isn't correct, we're out of sync
		{
			syncCounter++;

			if (syncCounter==5) // if we receive 5 bad syncs in a row, we need to resync 
			{
				spi_sync();
				cprintf("sync\n");
				syncCounter = 0;
			}
		}
		else
			syncCounter = 0;
	}
	else
	{
		// need to pump up the fifo because we only get an interrupt when fifo is half full
		// (and we won't receive data if we don't toggle SS)
		SS_NEGATE();
		SS_ASSERT();
	}
	
	return 0;
}
