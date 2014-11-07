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

#include <stdio.h>
#include "progvideo.h"
#include "pixy_init.h"
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
#ifdef DEFER
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
#endif
}

int videoSetup()
{
	g_loadModels = true;

	return 0;
}

void sendCustom(uint8_t renderFlags=RENDER_FLAG_FLUSH)
{
	int32_t len;
	uint8_t *frame = (uint8_t *)SRAM1_LOC;
	uint8_t cmodels[sizeof(ColorSignature)*NUM_MODELS];
	static int prevRes = CRP_RES_OK;
	uint32_t fcc;
	int res;


	if (g_execArg==1)
	{
		if (g_loadModels)
			loadColorModels(cmodels);

		// fill buffer contents manually for return data 
		len = Chirp::serialize(g_chirpUsb, frame, SRAM1_SIZE, HTYPE(FOURCC('C','M','V','1')), HINT8(renderFlags), FLTS32(g_loadModels ? sizeof(ColorSignature)*NUM_MODELS/sizeof(float) : 0, cmodels), UINT16(CAM_RES2_WIDTH), UINT16(CAM_RES2_HEIGHT), UINTS8_NO_COPY(CAM_RES2_WIDTH*CAM_RES2_HEIGHT), END);
		// write frame after chirp args
		cam_getFrame(frame+len, SRAM1_SIZE-len, CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT);

		// tell chirp to use this buffer
		res = g_chirpUsb->useBuffer(frame, len+CAM_RES2_WIDTH*CAM_RES2_HEIGHT); 

		g_loadModels = false;

		if (res==CRP_RES_OK && prevRes!=CRP_RES_OK) // force a reload
			g_loadModels = true;
		prevRes = res;
	}
	else if (100<=g_execArg && g_execArg<200)
	{
		fcc =  FOURCC('E','X',(g_execArg%100)/10 + '0', (g_execArg%10) + '0');
		len = Chirp::serialize(g_chirpUsb, frame, SRAM1_SIZE, HTYPE(fcc), HINT8(renderFlags), UINT16(CAM_RES2_WIDTH), UINT16(CAM_RES2_HEIGHT), UINTS8_NO_COPY(CAM_RES2_WIDTH*CAM_RES2_HEIGHT), END);
		// write frame after chirp args
		cam_getFrame(frame+len, SRAM1_SIZE-len, CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT);

		// tell chirp to use this buffer
		g_chirpUsb->useBuffer(frame, len+CAM_RES2_WIDTH*CAM_RES2_HEIGHT); 
	}
	else
		cam_getFrameChirp(CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT, g_chirpUsb);

}

int videoLoop()
{
	if (g_execArg==0)
		cam_getFrameChirp(CAM_GRAB_M1R2, 0, 0, CAM_RES2_WIDTH, CAM_RES2_HEIGHT, g_chirpUsb);
	else 
		sendCustom();
	return 0;
}

