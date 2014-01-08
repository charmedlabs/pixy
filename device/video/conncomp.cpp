#include <debug.h>
#include <string.h>
#include "pixy_init.h"
#include "camera.h"
#include "cameravals.h"
#include "conncomp.h"
#include "blobs.h"
#include "qqueue.h"


Qqueue *g_qqueue;
Blobs *g_blobs;

int g_loop = 0;

int32_t cc_servo(const uint32_t &start)
{
	g_loop = start;
	return 0;
}

static const ProcModule g_module[] =
{
	{
	"cc_getRLSFrame",
	(ProcPtr)cc_getRLSFrameChirp, 
	{END}, 
	"Get a frame of color run-length segments (RLS)"
	"@r 0 if success, negative if error"
	"@r CCQ1 formated data, including 8-palette"
	},
	{
	"cc_setModel",
	(ProcPtr)cc_setModel, 
	{CRP_UINT8, CRP_HTYPE(FOURCC('R','E','G','1')), END}, 
	"Set model by selecting box in image"
	"@p model numerical index of model, can be 0-5"
	"@p pixels user-selected pixels"
	"@r 0 if success, negative if error"
	},
	{
	"cc_setMemory",
	(ProcPtr)cc_setMemory,
	{CRP_UINT32, CRP_UINTS8, END},
	"" 
	},
	END
};

static ChirpProc g_getRLSFrameM0 = -1;


int cc_init(Chirp *chirp)
{
	g_qqueue = new Qqueue;
	g_blobs = new Blobs(g_qqueue);

	chirp->registerModule(g_module);	

	g_getRLSFrameM0 = g_chirpM0->getProc("getRLSFrame", NULL);

	if (g_getRLSFrameM0>0)
		return -1;

	return 0;
}

// this routine assumes it can grab valid pixels in video memory described by the box
int32_t cc_setModel(const uint8_t &model, const uint16_t &xoffset, const uint16_t &yoffset, const uint16_t &width, const uint16_t &height, Chirp *chirp)
{
	return 0;
}

int32_t cc_getRLSFrameChirp(Chirp *chirp)
{
	int32_t result;
	uint32_t len, numRls;

	g_qqueue->flush();

	// figure out prebuf length (we need the prebuf length and the number of runlength segments, but there's a chicken and egg problem...)
	len = Chirp::serialize(chirp, RLS_MEMORY, RLS_MEMORY_SIZE,  HTYPE(0), UINT16(0), UINT16(0), UINTS32_NO_COPY(0), END);

	result = cc_getRLSFrame((uint32_t *)(RLS_MEMORY+len), LUT_MEMORY);
	// copy from IPC memory to RLS_MEMORY
	numRls = g_qqueue->readAll((Qval *)(RLS_MEMORY+len), (RLS_MEMORY_SIZE-len)/sizeof(Qval));
	Chirp::serialize(chirp, RLS_MEMORY, RLS_MEMORY_SIZE,  HTYPE(FOURCC('C','C','Q','1')), UINT16(CAM_RES2_WIDTH), UINT16(CAM_RES2_HEIGHT), UINTS32_NO_COPY(numRls), END);
	// send frame, use in-place buffer
	chirp->useBuffer(RLS_MEMORY, len+numRls*4);

	return result;
}

int32_t cc_getRLSFrame(uint32_t *memory, uint8_t *lut, bool sync)
{
	int32_t res;
	int32_t responseInt = -1;

	// check mode, set if necessary
	if ((res=cam_setMode(CAM_MODE1))<0)
		return res;

	// forward call to M0, get frame
	if (sync)
	{
		g_chirpM0->callSync(g_getRLSFrameM0, 
			UINT32((uint32_t)memory), UINT32((uint32_t)lut), END_OUT_ARGS,
			&responseInt, END_IN_ARGS);
		return responseInt;
	}
	else
	{
		g_chirpM0->callAsync(g_getRLSFrameM0, 
			UINT32((uint32_t)memory), UINT32((uint32_t)lut), END_OUT_ARGS);
		return 0;
	}

}

int32_t cc_setMemory(const uint32_t &location, const uint32_t &len, const uint8_t *data)
{
	uint32_t i;
	uint8_t *dest = (uint8_t *)location;
	for (i=0; i<len; i++)
		dest[i] = data[i];

	return len;
}

