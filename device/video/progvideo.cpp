#include "progvideo.h"
#include "pixy_init.h"
#include "camera.h"

 
Program g_progVideo =
{
	"video",
	"continuous stream of raw camera frames",
	videoSetup, 
	videoLoop
};

int videoSetup()
{
	return 0;
}

int videoLoop()
{
	static int i = 0;
	cprintf("hello %d\n", i++);
	cam_getFrameChirp(0x21, 0, 0, 320, 200, g_chirpUsb);

	return 0;
}

