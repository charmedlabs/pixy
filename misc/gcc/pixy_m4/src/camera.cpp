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

#include <pixy_init.h>
#include <pixyvals.h>
#include "camera.h"
#include "param.h"

static const ProcModule g_module[] =
{
	{
	"cam_setMode",
	(ProcPtr)cam_setMode, 
	{CRP_INT8, END}, 
	"Set camera mode"
	"@p mode 0=25 FPS, 1280x800; 1=50 FPS, 640x400"
	"@r 0 if success, negative if error"
	},
	{
	"cam_getMode", 
	(ProcPtr)cam_getMode, 
	{END},
	"Get camera mode"
	"@r mode value"
	},
	{
	"cam_setAWB",
	(ProcPtr)cam_setAWB, 
	{CRP_INT8, END}, 
	"Enable/disable Auto White Balance (AWB)" 
	"@p enable (bool) 0=disable, 1=enable"
	"@r 0 if success, negative if error"
	},
	{
	"cam_getAWB", 
	(ProcPtr)cam_getAWB, 
	{END},
	"Get Auto White Balance (AWB) enable"
	"@r (bool) enable value"
	},
	{
	"cam_setWBV", 
	(ProcPtr)cam_setWBV, 
	{CRP_INT32, END},
	"Set White Balance Value (WBV)"
	"@p wbv white balance value"
	"@r 0 if success, negative if error"
	},
	{
	"cam_getWBV", 
	(ProcPtr)cam_getWBV, 
	{END},
	"Get White Balance Value (WBV)"
	"@r white balance value"
	},
	{
	"cam_setAEC", 
	(ProcPtr)cam_setAEC, 
	{CRP_INT8, END},
	"Set Auto Exposure Compensation (AEC)"
	"@p enable (bool) 0=disable, 1=enable"
	"@r 0 if success, negative if error"
	},
	{
	"cam_getAEC", 
	(ProcPtr)cam_getAEC, 
	{END},
	"Get Auto Exposure Compensation (AEC)"
	"@r (bool) enable value"
	},
	{
	"cam_setECV", 
	(ProcPtr)cam_setECV, 
	{CRP_INT32, END},
	"Set Exposure Compensation Value (ECV)"
	"@p exposure compensation value"
	"@r 0 if success, negative if error"
	},
	{
	"cam_getECV", 
	(ProcPtr)cam_getECV, 
	{END},
	"Get Exposure Compensation Value (ECV)"
	"@r exposure compensation value"
	},
	{
	"cam_setBrightness", 
	(ProcPtr)cam_setBrightness, 
	{CRP_INT8, END},
	"Set brightness value to increase or decrease exposure (only applies when AEC is enabled)"
	"@p brightness value between 0 and 255"
	"@r 0 if success, negative if error"
	},
	{
	"cam_getBrightness", 
	(ProcPtr)cam_getBrightness, 
	{END},
	"Get brightness value"
	"@r brightness value"
	},
	{
	"cam_setLightMode", 
	(ProcPtr)cam_setLightMode, 
	{CRP_INT8, END},
	"Set light mode to compensate for low or high lighting conditions"
	"@p mode one of the following: CAM_LIGHT_NORMAL (0), CAM_LIGHT_LOW (1), CAM_LIGHT_HIGH (2)"
	"@r 0 if success, negative if error"
	},
	{
	"cam_getLightMode", 
	(ProcPtr)cam_getLightMode, 
	{END},
	"Get light mode"
	"@r 0 if success, negative if error"
	},
	{
	"cam_testPattern", 
	(ProcPtr)cam_testPattern, 
	{CRP_INT8, END},
	"Set test pattern display"
	"@p (0) normal mode (1) test pattern"
	"@r always returns 0"
	},
	{
	"cam_getFrame", 
	(ProcPtr)cam_getFrameChirp, 
	{CRP_INT8, CRP_INT16, CRP_INT16, CRP_INT16, CRP_INT16, END},
	"Get a frame from the camera"
	"@p mode one of the following CAM_GRAB_M0R0 (0x00), CAM_GRAB_M1R1 (0x11), CAM_GRAB_M1R2 (0x21)"
	"@p xOffset x offset counting from left"
	"@p yOffset y offset counting from top"
	"@p width width of frame"
	"@p height height of frame"
	"@r 0 if success, negative if error"
	"@r BA81 formatted data"
	},
	{
	"cam_setRegister",
	(ProcPtr)cam_setRegister,
	{CRP_INT8, CRP_INT8, END},
	"Write an SCCB register value on the camera chip"
	"@p address register address"
	"@p value register value to set"
	"@r 0 if success, negative if error"
	},  
	{
	"cam_getRegister",
	(ProcPtr)cam_getRegister,
	{CRP_INT8, END},
	"Read an SCCB register on the camera chip"
	"@p address register address"
	"@r 0 register value"
	},  
	END
};

CSccb *g_sccb = NULL;
Frame8 g_rawFrame;

static uint8_t g_mode = (uint8_t)-1;
static uint8_t g_awb = 1;
static uint8_t g_aec = 1;
static uint8_t g_lightMode = 0;
static uint8_t g_brightness = CAM_BRIGHTNESS_DEFAULT;
static ChirpProc g_getFrameM0 = -1;

static const uint8_t g_baseRegs[] =
{
	0x12, 0x80, // reset regs
	// set clock 20.4 * 8/4 = 40.8 MHz -> 29.14 fps.  
	// If we go to 6 pclks per cpu clk, 25.7 fps	0x12, 0x80, 
	0x5c, 32-4, // set multiplier (32-val) 
	0x11, 0x00, // set divider (1+val)*2
	0xc3, 0x22,	// set 8-bit mode (instead of 10 bit.  

    //Core Settings
    0x1e, 0x07, // default 0x00 reserved
    0x5f, 0x18, // default 0x00 not listed
    0x65, 0x2a, // default 0x1a not listed
    0x68, 0x0a, // default 0x07 not listed
    0x4d, 0x90, // default 0x10 reserved
    0xc1, 0x80, // default 0x00 yavg_winofh
    0x0c, 0x30, // default 0x00 reserved
    0x6d, 0x02, // default 0x82 not listed
    0x96, 0xf1, // default 0xf9 function enable/disable-- disable lens correction

    //Resolution and Format
    0x17, 0x25, // default 0x26 sensor horizontal output start msbs
    0x32, 0x07, // default 0x01 sensor horizontal start lsbs
    0x26, 0xf1, // default 0xd2 fast mode large step range 
	// note, above value may have something to do with agc oscillation
	// 0x26=0xa1, noticed oscillation, which was improved when set back to
    // default (0x26=0xd2)

    //Clock
    0x2a, 0x98, // default 0x9b ?	horiz Tp counter endpoint lsbs

    //General
    0x13, 0xa5, // default 0x85 (set banding filter on)
    0x14, 0x88, // default 0x40 bit 4 reserved 	;Gain Ceiling 8X

    //Banding
    0x22, 0x03, // default 0x00 max smooth banding steps
};

static const uint8_t g_mode0Regs[] =
{
    //Resolution and Format
	0x12, 0x00, // default 0x00 timing register, vert, horiz subsample
	0x3b, 0x00, // default 0x00 reserved	
	0x1a, 0xc8, // default 0xc8 sensor vertical output size msbs
	0x03, 0x0A, // default 0x02 (bit 4 reserved)
	0x58, 0xc8, // default 0xc8 (this sets to 720 lines)
	0x59, 0xA0, // default 0xa0 ?
 	0x4c, 0x13, // default 0x13 reserved
	0x4b, 0x36, // default 0x36 reserved
 	0x3d, 0x3c, // default 0x3c ?
	0x3e, 0x03, // default 0x03 ?
  	0xbd, 0xa0, // default 0xa0 ?
	0xbe, 0xc8, // default 0xc8 yavg_winv
	0x2c, 0x50,
	0x23, 0x00,

	//Banding
	0x49, 0xce // default 0xcf banding step lsbs
	};

static const uint8_t g_mode1Regs[] =
{
	 //Resolution and Format
	0x12, 0x40,
	0x3b, 0x01,
	0x1a, 0x64,
	0x03, 0x02,
	0x58, 0x64,
	0x59, 0x50,
	0x4b, 0x9a,
	0x4c, 0x09,
	0x3d, 0x9e,
	0x3e, 0x01,
	0xbd, 0x50,
	0xbe, 0x64,
	0x2c, 0x60,
	0x23, 0x10,

	//Banding
	0x49, 0x67,
};

static void cam_setRegs(const uint8_t *rPairs, int len);

int cam_init()
{
	g_sccb = new CSccb(0x60);

	// flush sccb
	g_sccb->Read(0xA0);

	// start in mode 0
	cam_setRegs(g_baseRegs, sizeof(g_baseRegs));
	cam_setMode(0);
	
	g_chirpUsb->registerModule(g_module);
	
	g_getFrameM0 = g_chirpM0->getProc("getFrame", NULL);

	if (g_getFrameM0<0)
		return -1;

	cam_loadParams();

	return 0;
}

int32_t cam_setMode(const uint8_t &mode)
{
	if (mode!=g_mode)
	{
		if (mode==0)
		{
			cam_setRegs(g_mode0Regs, sizeof(g_mode0Regs));
			g_mode = 0;
		}
		else if (mode==1)
		{
			cam_setRegs(g_mode1Regs, sizeof(g_mode1Regs));
			g_mode = 1;
		}
		else 
			return -1;
	}
	return 0;
}

uint32_t cam_getMode()
{
	return g_mode;
}

int32_t cam_setAWB(const uint8_t &awb)
{
	if (awb!=g_awb)
	{
		if (awb==0)
		{
			g_sccb->Write(0x38, 0x00);
			g_sccb->Write(0x96, 0xe1);
			g_awb = 0;
		}
		else
		{
			g_sccb->Write(0x38, 0x10);
			g_sccb->Write(0x96, 0xf1);
			g_awb = 1;
		}
   	}
	return 0;
}

uint32_t cam_getAWB()
{
	return g_awb;
}
							   
int32_t cam_setWBV(const uint32_t &wbv)
{
	uint32_t val = wbv;
	g_sccb->Write(0x05, (unsigned char)(val&0xff));	// green
	val >>= 8;
	g_sccb->Write(0x02, (unsigned char)(val&0xff)); // red
	val >>= 8;
	g_sccb->Write(0x01, (unsigned char)(val&0xff)); // blue
	
	return 0;			
}

uint32_t cam_getWBV()
{
	uint32_t wbv;

	wbv = g_sccb->Read(0x01); // blue
	wbv <<= 8;
	wbv |= g_sccb->Read(0x02); // red
	wbv <<= 8;
	wbv |= g_sccb->Read(0x05); // green

	return wbv;
}


int32_t cam_setAEC(const uint8_t &aec)
{
	if (aec!=g_aec)
	{
		if (aec==0)
		{
			g_sccb->Write(0x13, 0xa0); // turn off AEC, AGC
			g_aec = 0;
		}
		else
		{
			g_sccb->Write(0x13, 0xa5); // enable AEC, AGC
			g_aec = 1;
		}
   	}
	return 0;
}

uint32_t cam_getAEC()
{
	return g_aec;
}

int32_t cam_setECV(const uint32_t &ecv)
{
	uint32_t val = ecv;

	g_sccb->Write(0x00, (unsigned char)(val&0xff));	// AGC gain
	val >>= 8;
	g_sccb->Write(0x10, (unsigned char)(val&0xff));	// AEC LSB
	val >>= 8;
	g_sccb->Write(0x16, (unsigned char)(val&0xff)); // AEC MSB	
	
	return 0;		
}

uint32_t cam_getECV()
{
	uint32_t ecv;

	ecv = g_sccb->Read(0x16); // AEC MSB
	ecv <<= 8;
	ecv |= g_sccb->Read(0x10); // AEC LSB
	ecv <<= 8;
	ecv |= g_sccb->Read(0x00); // AGC gain

	return ecv;
}

int32_t cam_setBrightness(const uint8_t &brightness)
{
	g_sccb->Write(0x24, brightness); 
	g_sccb->Write(0x25, brightness>CAM_BRIGHTNESS_RANGE?brightness-CAM_BRIGHTNESS_RANGE:0);
	g_brightness = brightness;

	return 0;
}

uint32_t cam_getBrightness()
{
	return g_brightness;
}

int32_t cam_setLightMode(const uint8_t &mode)
{
	uint8_t val13, val03;

	val13 = g_sccb->Read(0x13);
	val03 = g_sccb->Read(0x03);

	if (mode!=g_lightMode)
	{
		if (mode==CAM_LIGHT_NORMAL)	// note, it seems that once you enable VAEC, you can't disable by writing to 0x0e (you can issue a reset though)
		{
			g_sccb->Write(0x13, val13&~0x08); // disable LAEC
			g_sccb->Write(0x03, val03&~0x80); // set maxframes to normal
			g_sccb->Write(0x0e, 0x40);        // disable VAEC
			g_sccb->Write(0x21, 0x03);			
		}
		else if (mode==CAM_LIGHT_LOW)
		{
			g_sccb->Write(0x13, val13&~0x08); // disable LAEC
			g_sccb->Write(0x03, val03|0x80);  // max frames for VAEC
			g_sccb->Write(0x0e, 0x48);        // enable VAEC
			g_sccb->Write(0x21, 0x33);        // set VAEC trigger point to 16x gain or greater
		}
		else if (mode==CAM_LIGHT_HIGH)
			g_sccb->Write(0x13, val13|0x08);  // enable LAEC
		else 
			return -1;
	}

	g_lightMode = mode;
	return 0; 
}

uint32_t cam_getLightMode()
{
	return g_lightMode;
}

int cam_testPattern(const uint8_t &enable)
{
	uint8_t val;

	val = g_sccb->Read(0x97); 
	if (enable)
		g_sccb->Write(0x97, val | 0x0a);
	else
		g_sccb->Write(0x97, val & ~0x0a);

	return 0;
}

int32_t cam_getFrame(uint8_t *memory, uint32_t memSize, uint8_t type, uint16_t xOffset, uint16_t yOffset, uint16_t xWidth, uint16_t yWidth)
{
	int32_t res;
	int32_t responseInt = -1;

	if (xWidth*yWidth>memSize)
		return -2;

	// check resolutions
	res = type >> 4;
	if (res==0)
	{
		if (xOffset+xWidth>CAM_RES0_WIDTH || yOffset+yWidth>CAM_RES0_HEIGHT)
			return -1;
	}
	else if (res==1) 
	{
		if (xOffset+xWidth>CAM_RES1_WIDTH || yOffset+yWidth>CAM_RES1_HEIGHT)
			return -1;
	}
	else if (res==2)
	{
		if (xOffset+xWidth>CAM_RES2_WIDTH || yOffset+yWidth>CAM_RES2_HEIGHT)
			return -1;
	}
	else
		return -3;

	// check mode, set if necessary
	if ((res=cam_setMode(type&0x0f))<0)
		return res;

	// forward call to M0, get frame
	g_chirpM0->callSync(g_getFrameM0, 
		UINT8(type), UINT32((uint32_t)memory), UINT16(xOffset), UINT16(yOffset), UINT16(xWidth), UINT16(yWidth), END_OUT_ARGS,
		&responseInt, END_IN_ARGS);

	if (responseInt==0)
	{
		g_rawFrame.m_pixels = memory;
		g_rawFrame.m_width = xWidth;
		g_rawFrame.m_height = yWidth;
	}

	return responseInt;
}

int32_t cam_getFrameChirp(const uint8_t &type, const uint16_t &xOffset, const uint16_t &yOffset, const uint16_t &xWidth, const uint16_t &yWidth, Chirp *chirp)
{
	return cam_getFrameChirpFlags(type, xOffset, yOffset, xWidth, yWidth, chirp);
}

int32_t cam_getFrameChirpFlags(const uint8_t &type, const uint16_t &xOffset, const uint16_t &yOffset, const uint16_t &xWidth, const uint16_t &yWidth, Chirp *chirp, uint8_t renderFlags)
{
	int32_t result, len;
	uint8_t *frame = (uint8_t *)SRAM1_LOC;

	// fill buffer contents manually for return data 
	len = Chirp::serialize(chirp, frame, SRAM1_SIZE, HTYPE(FOURCC('B','A','8','1')), HINT8(renderFlags), UINT16(xWidth), UINT16(yWidth), UINTS8_NO_COPY(xWidth*yWidth), END);
	// write frame after chirp args
	result = cam_getFrame(frame+len, SRAM1_SIZE-len, type, xOffset, yOffset, xWidth, yWidth);

	// tell chirp to use this buffer
	chirp->useBuffer(frame, len+xWidth*yWidth); 

	return result;
}

int32_t cam_setRegister(const uint8_t &reg, const uint8_t &value)
{
  	g_sccb->Write(reg, value);
	return 0;
}

int32_t cam_getRegister(const uint8_t &reg)
{
	return g_sccb->Read(reg);
}

void cam_setRegs(const uint8_t *rPairs, int len)
{
	int i;

	// put imager in sleep mode
  	g_sccb->Write(0x09, 0x10);
	for (i=0; i<len; i+=2)
	{
		//printf("0x%x = 0x%x\n", rPairs[i], g_sccb->Read(rPairs[i]));
		g_sccb->Write(rPairs[i], rPairs[i+1]);
		if (g_sccb->Read(rPairs[i])!=rPairs[i+1])
			volatile int q = 0;
	}
	// take imager out of sleep mode
  	g_sccb->Write(0x09, 0x00);

	cam_setBrightness(g_brightness);
}

void cam_loadParams()
{
	prm_add("Brightness", 0, 
		"Sets the average brightness of the camera, can be between 0 and 255 (default 90)", UINT8(CAM_BRIGHTNESS_DEFAULT), END);

	uint8_t brightness;
	prm_get("Brightness", &brightness, END);
	cam_setBrightness(brightness);
}
