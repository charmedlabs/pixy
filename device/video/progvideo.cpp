#include "progvideo.h"
#include "pixy_init.h"
#include <stdio.h>
#include "camera.h"
//#include "colorlut.h"
#include "blobs.h"
#include "param.h"
#include <string.h>

static bool g_loadModels;
 
Program g_progVideo =
{
	"video",
	"continuous stream of raw camera frames",
	videoSetup, 
	videoLoop
};

void loadColorModels(uint8_t *cmodels)
{
	int i;
	uint32_t len;
	char id[32];
	ColorModel *cmodel;

	for (i=1; i<=NUM_MODELS; i++, cmodels+=sizeof(ColorModel))
	{
		sprintf(id, "signature%d", i);
		// get signature and add to color lut
		prm_get(id, &len, &cmodel, END);
		memcpy(cmodels, cmodel, sizeof(ColorModel));
	}
}

int videoSetup()
{
	g_loadModels = true;

	return 0;
}

void sendCMV1(uint8_t renderFlags=RENDER_FLAG_FLUSH)
{
	int32_t len;
	uint8_t *frame = (uint8_t *)SRAM1_LOC;
	uint8_t cmodels[sizeof(ColorModel)*NUM_MODELS];
	static int prevRes = CRP_RES_OK;
	int res;

	if (g_loadModels)
		loadColorModels(cmodels);

	// fill buffer contents manually for return data 
	len = Chirp::serialize(g_chirpUsb, frame, SRAM1_SIZE, HTYPE(FOURCC('C','M','V','1')), HINT8(renderFlags), UINTS8(g_loadModels ? sizeof(ColorModel)*NUM_MODELS : 0, cmodels), UINT16(CAM_RES2_WIDTH), UINT16(CAM_RES2_HEIGHT), UINTS8_NO_COPY(CAM_RES2_WIDTH*CAM_RES2_HEIGHT), END);
	// write frame after chirp args
	cam_getFrame(frame+len, SRAM1_SIZE-len, CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT);

	// tell chirp to use this buffer
	res = g_chirpUsb->useBuffer(frame, len+CAM_RES2_WIDTH*CAM_RES2_HEIGHT); 

	g_loadModels = false;

	if (res==CRP_RES_OK && prevRes!=CRP_RES_OK) // force a reload
		g_loadModels = true;
	prevRes = res;
}

int videoLoop()
{
	if (g_execArg==0)
		cam_getFrameChirp(CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT, g_chirpUsb);
	else 
		sendCMV1();
	return 0;
}

