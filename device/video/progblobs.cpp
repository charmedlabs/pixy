#include "progblobs.h"
#include "pixy_init.h"
#include "camera.h"
#include "qqueue.h"
#include "blobs.h"

extern Qqueue *g_qqueue;
extern Blobs *g_blobs;

Program g_progBlobs =
{
	"blobs",
	"perform color blob analysis",
	blobsSetup, 
	blobsLoop
};


int blobsSetup()
{
	cam_setMode(CAM_MODE1);
 	
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
	g_blobs->blobify();
#endif
	
	return 0;
}
