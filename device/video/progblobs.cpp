#include "progblobs.h"
#include "pixy_init.h"
#include "camera.h"
#include "qqueue.h"

extern Qqueue *g_qqueue;

Program g_progBlobs =
{
	"blobs",
	"detect colored objects and return them as blobs",
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
	static int i=0;
	Qval qval;

	while(g_qqueue->dequeue(&qval))
	{
		if (qval==0xffffffff)
			cprintf("%d\n", i++);	
	}

	return 0;
}
