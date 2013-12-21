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
	{
	"cc_getRLSCC",
	(ProcPtr)cc_getRLSCCChirp,
	{END},
	"Gets the bounding boxes of connected components using run-lengths"
	},
	{
	"cc_servo",
	(ProcPtr)cc_servo,
	{CRP_UINT32, END},
	"start servoing"
	},
	END
};

static ChirpProc g_getRLSFrameM0 = -1;


int cc_init(Chirp *chirp)
{
	uint32_t i;

	g_qqueue = new Qqueue;
	g_blobs = new Blobs(g_qqueue);

	chirp->registerModule(g_module);	

	g_getRLSFrameM0 = g_chirpM0->getProc("getRLSFrame", NULL);

	// clear lut
	for (i=0; i<LUT_MEMORY_SIZE; i++)
		LUT_MEMORY[i] = 0;

	if (g_getRLSFrameM0>0)
		return -1;

	return 0;
}

void RGBtoHSV(uint8_t r, uint8_t g, uint8_t b, uint8_t *h, uint8_t *s, uint8_t *v, uint8_t *c)
{
    uint8_t min, max, delta;
    int hue;
    min = MIN(r, g);
    min = MIN(min, b);
    max = MAX(r, g);
    max = MAX(max, b);

    *v = max;
    delta = max - min;
    if (max!=0)
        *s = ((int)delta<<8)/max;
    if (max==0 || delta==0)
    {
        *s = 0;
        *h = 0;
        *c = 0;
        return;
    }
    if (r==max)
        hue = (((int)g - (int)b)<<8)/delta;         // between yellow & magenta
    else if (g==max)
        hue = (2<<8) + (((int)b - (int)r)<<8)/delta;     // between cyan & yellow
    else
        hue = (4<<8) + (((int)r - (int)g)<<8)/delta;     // between magenta & cyan
    if(hue < 0)
        hue += 6<<8;
    hue /= 6;
    *h = hue;
    *c = delta;
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

	// figure out prebuf length (we need the prebuf length and the number of runlength segments, but there's a chicken and egg problem...)
	len = Chirp::serialize(chirp, RLS_MEMORY, RLS_MEMORY_SIZE,  HTYPE(0), UINT16(0), UINT16(0), UINTS32_NO_COPY(0), END);

	result = cc_getRLSFrame((uint32_t *)(RLS_MEMORY+len), LUT_MEMORY, &numRls);
	// send frame, use in-place buffer
	Chirp::serialize(chirp, RLS_MEMORY, RLS_MEMORY_SIZE,  HTYPE(FOURCC('C','C','Q','1')), UINT16(CAM_RES2_WIDTH), UINT16(CAM_RES2_HEIGHT), UINTS32_NO_COPY(numRls), END);
	// copy from IPC memory to RLS_MEMORY
	if (g_qqueue->readAll((Qval *)(RLS_MEMORY+len), (RLS_MEMORY_SIZE-len)/sizeof(Qval))!=numRls)
		return -1;
	chirp->useBuffer(RLS_MEMORY, len+numRls*4);

	return result;
}

int32_t cc_getRLSFrame(uint32_t *memory, uint8_t *lut, uint32_t *numRls, bool sync)
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
			&responseInt, numRls, END_IN_ARGS);
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

int32_t cc_getRLSCCChirp(Chirp *chirp)
{
	
	int16_t* c_components = new int16_t[MAX_BLOBS*4];
	
	
	uint32_t numRls, result;//, prebuf;
	uint32_t *memory = (uint32_t *)RLS_MEMORY;
	result = cc_getRLSFrame(memory, LUT_MEMORY, &numRls);
	
	CBlobAssembler blobber;
	
	int32_t row;
	uint32_t i, startCol, length;
	uint8_t model;

	for (i=0, row=-1; i<numRls; i++)
	{
			if (memory[i]==0)
			{
					row++;
					continue;
			}
			model = memory[i]&0x03;
			memory[i] >>= 3;
			startCol = memory[i]&0x1ff;
			memory[i] >>= 9;
			length = memory[i]&0x1ff;
			if(!handleRL(&blobber, model, row, startCol, length))
				break;
	}
	
	blobber.EndFrame();
	blobber.SortFinished();
	
	//
	// Take Finished blobs and return with chirp
	//
	CBlob *blob, *temp;
	blob = blobber.finishedBlobs;
	
	uint32_t cc_num = 0;
	temp = blob;
	while (temp)
	{
		int16_t top, right, bottom, left;
		temp->getBBox(left, top, right, bottom);
		
		// Don't want objects with area less than 9...
		if ((right-left)*(bottom-top) < 9)
			break;
		
		temp = temp->next;
		cc_num++;
	}
	
	// Remove the rest that we don't care about
	/*while(temp)
	{
		CBlob *next = temp->next;
		temp->~CBlob();
		temp = NULL;
		temp = next;
	}*/
	
	cc_num = (cc_num < 15) ? cc_num : MAX_BLOBS;

	// Populate return w/ result
	//void* mem = malloc(sizeof(int16_t)*cc_num*4);
	//if (mem == NULL)
	//	int i = 0;
	//free(mem);
	//int16_t* c_components = new int16_t[cc_num*4];
	//g_mem += sizeof(int16_t)*cc_num*4;
	memset((void *)c_components, 0, sizeof(uint16_t)*cc_num*4);

	for (int i = 0; i < cc_num; i++)
	{
		int16_t top, right, bottom, left;
		blob->getBBox(left, top, right, bottom);
		c_components[(i*4)+0] = top;
		c_components[(i*4)+1] = right;
		c_components[(i*4)+2] = bottom;
		c_components[(i*4)+3] = left;

		blob = blob->next;
	}
	
	//CRP_RETURN(chirp, USE_BUFFER(SRAM0_SIZE, SRAM0_LOC), HTYPE(0), UINT16(0), UINT16(0), UINTS8(0, 0), END);
	//prebuf = chirp->getPreBufLen();
	
	blobber.Reset();
	
	CRP_RETURN(chirp, HTYPE(FOURCC('V','I','S','U')), UINTS16(cc_num*4, c_components), END);
	
	delete[] c_components;
	//g_mem -= sizeof(int16_t)*cc_num*4;

	return result;
}

int32_t cc_getMaxBlob(uint32_t *qvals, uint32_t numRls, int16_t *bdata)
{
	
	int16_t* c_components = new int16_t[MAX_BLOBS*4];
	
	uint32_t result;//, prebuf;
	
	CBlobAssembler blobber;
	
	int32_t row;
	uint32_t i, startCol, length;
	uint8_t model;

	for (i=0, row=-1; i<numRls; i++)
	{
			if (qvals[i]==0)
			{
					row++;
					continue;
			}
			model = qvals[i]&0x03;
			qvals[i] >>= 3;
			startCol = qvals[i]&0x1ff;
			qvals[i] >>= 9;
			length = qvals[i]&0x1ff;
			if(!handleRL(&blobber, model, row, startCol, length))
				break;
	}
	
	blobber.EndFrame();
	blobber.SortFinished();

	int16_t top, right, bottom, left;
	CBlob *blob;
	blob = blobber.finishedBlobs;
	if (blob->GetArea()>MIN_AREA)
	{
		blob->getBBox(left, top, right, bottom);
		bdata[0] = left;
		bdata[1] = right;
		bdata[2] = top;
		bdata[3] = bottom;
	}
	else 
		bdata[0] = -1;

#if 0	
	//
	// Take Finished blobs and return with chirp
	//
	CBlob *blob, *temp;
	blob = blobber.finishedBlobs;
	
	uint32_t cc_num = 0;
	temp = blob;
	while (temp)
	{
		int16_t top, right, bottom, left;
		temp->getBBox(left, top, right, bottom);
		
		// Don't want objects with area less than 9...
		if ((right-left)*(bottom-top) < 9)
			break;
		
		temp = temp->next;
		cc_num++;
	}
	
	// Remove the rest that we don't care about
	/*while(temp)
	{
		CBlob *next = temp->next;
		temp->~CBlob();
		temp = NULL;
		temp = next;
	}*/
	
	cc_num = (cc_num < 15) ? cc_num : MAX_BLOBS;

	// Populate return w/ result
	//void* mem = malloc(sizeof(int16_t)*cc_num*4);
	//if (mem == NULL)
	//	int i = 0;
	//free(mem);
	//int16_t* c_components = new int16_t[cc_num*4];
	//g_mem += sizeof(int16_t)*cc_num*4;
	memset((void *)c_components, 0, sizeof(uint16_t)*cc_num*4);

	for (int i = 0; i < cc_num; i++)
	{
		int16_t top, right, bottom, left;
		blob->getBBox(left, top, right, bottom);
		c_components[(i*4)+0] = top;
		c_components[(i*4)+1] = right;
		c_components[(i*4)+2] = bottom;
		c_components[(i*4)+3] = left;

		blob = blob->next;
	}
	
	//CRP_RETURN(chirp, USE_BUFFER(SRAM0_SIZE, SRAM0_LOC), HTYPE(0), UINT16(0), UINT16(0), UINTS8(0, 0), END);
	//prebuf = chirp->getPreBufLen();
#endif
	
	blobber.Reset();
		
	delete[] c_components;
	//g_mem -= sizeof(int16_t)*cc_num*4;

	return result;
}

int handleRL(CBlobAssembler *blobber, uint8_t model, int row, int startCol, int len)
{
	if (startCol < 0)
		return 0;

	SSegment s;
	s.model = 0;
	s.row = row;
	s.startCol = startCol;
	s.endCol = startCol + len;
	if (!blobber->Add(s))
		return 0;
	return 1;
}
