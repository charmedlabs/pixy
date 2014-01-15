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
	cam_getFrameChirp(CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT, g_chirpUsb);

	return 0;
}

