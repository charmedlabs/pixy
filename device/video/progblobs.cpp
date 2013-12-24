#include "progblobs.h"
#include "pixy_init.h"
#include "camera.h"
#include "qqueue.h"
#include "blobs.h"
#include "spi.h"

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
 	
	// setup qqueue and M0
	g_qqueue->flush();
	exec_runM0(0);

	return 0;
}

int blobsLoop()
{
#if 0
	static int i=0;
	Qval qval;

	while(g_qqueue->dequeue(&qval))
	{
		if (qval==0xffffffff)
			cprintf("%d\n", i++);	
	}
#else
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
#endif
	
	return 0;
}
