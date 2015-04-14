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

#include <string.h>
#include "progblobs.h"
#include "pixy_init.h"
#include "camera.h"
#include "led.h"
#include "conncomp.h"
#include "serial.h"
#include "rcservo.h"
#include "exec.h"

#define LEGO

bool g_ledSet = false;
static uint8_t g_state=0;

Program g_progBlobs =
{
	"blobs",
	"perform color blob analysis",
	blobsSetup, 
	blobsLoop
};

#ifdef LEGO
void lego_handleRecv();
uint8_t g_legoRequest;

uint16_t lego_getData(uint8_t *buf, uint32_t buflen)
{
	uint8_t c;
	uint16_t d;
	uint16_t numBlobs;
	uint32_t temp, width, height;
	Iserial *serial = ser_getSerial();

	if (serial->receive(&c, 1)==0)
		return 0;

#if 0
	if (c==0x00)
	{
		char *str = "V0.1      ";
		strcpy((char *)buf, str);
		return 8;
		//return strlen((char *)str);
	}
	if (c==0x08)
	{
		char *str = "Pixy      ";
		strcpy((char *)buf, str);
		return 8;
		//return strlen((char *)str);
	}
	else if (c==0x10)
	{
		char *str = "Vision    ";
		strcpy((char *)buf, str);
		return 8;
		//return strlen((char *)str);
	}
	else 
#endif
	if (c>=0x51 && c<=0x57)
	{
#if 1
		buf[0] = 0;
		buf[1] = 1;
		buf[2] = 2;
		buf[3] = 3;
		buf[4] = 4;
#else
		BlobA *max;
		max = g_blobs->getMaxBlob(c-0x50, &numBlobs);
		if (max==0)
			memset(buf, 0, 5);
		else if (max==(BlobA *)-1)
			memset(buf, -1, 5);
		else
		{
			width = max->m_right - max->m_left;
			height = max->m_bottom - max->m_top;
			buf[0] = numBlobs;
			temp = ((max->m_left + width/2)*819)>>10;
			buf[1] = temp;
			temp = ((max->m_top + height/2)*819)>>10;
			buf[2] = temp;
			temp = (width*819)>>10;
			buf[3] = temp;
			temp = (height*819)>>10;
			buf[4] = temp;
		}
#endif
		return 5;
	}
	else if (c==0x58)
	{
		BlobB *max;
		if (serial->receive((uint8_t *)&d, 2)<2)
			return 0;
		max = (BlobB *)g_blobs->getMaxBlob(d, &numBlobs);
		if (max==0)
			memset(buf, 0, 6);
		else if (max==(BlobB *)-1)
			memset(buf, -1, 6);
		else
		{
			width = max->m_right - max->m_left;
			height = max->m_bottom - max->m_top;
			buf[0] = numBlobs;
			temp = ((max->m_left + width/2)*819)>>10;
			buf[1] = temp;
			temp = ((max->m_top + height/2)*819)>>10;
			buf[2] = temp;
			temp = (width*819)>>10;
			buf[3] = temp;
			temp = (height*819)>>10;
			buf[4] = temp;
			temp = ((int32_t)max->m_angle*91)>>7;
			buf[5] = temp;
		}
		return 6;
	}
	else  
	{
		BlobA *max;
		max = g_blobs->getMaxBlob();
		if (max==0 || max==(BlobA *)-1)
			buf[0] = 0;
		else
		{
			width = max->m_right - max->m_left;
			temp = ((max->m_left + width/2)*819)>>10;
			buf[0] = temp;
		}
		return 1;
	}
}

void lego_handleRecv()
{
	return;
	uint8_t a;
	Iserial *serial = ser_getSerial();

	while(serial->receive(&a, 1))
	{
		cprintf("%x\n", a);
		g_legoRequest = a;
	}
	return;
}

#endif


int blobsSetup()
{
	uint8_t c;

	// setup camera mode
	cam_setMode(CAM_MODE1);

	// if there have been any parameter changes, we should regenerate the LUT (do it regardless)
	g_blobs->m_clut.generateLUT();	
			
	// setup qqueue and M0
	g_qqueue->flush();
	exec_runM0(0);

	// flush serial receive queue
	while(ser_getSerial()->receive(&c, 1));

	g_state = 0; // reset recv state machine
	return 0;
}

void handleRecv()
{
	uint8_t i, a;
	static uint16_t w=0xffff;
	static uint8_t lastByte;
	uint16_t s0, s1;
	Iserial *serial = ser_getSerial();

	for (i=0; i<10; i++)
	{
		switch(g_state)
		{	
		case 0: // reset 
			lastByte = 0xff;  // This is not part of any of the sync word most significant bytes
			g_state = 1;
		 	break;

		case 1:	// sync word
			if(serial->receive(&a, 1))
			{
				w = lastByte << 8;
				w |= a;
				lastByte = a;
				g_state = 2;	// compare
			}
			break;

		case 2:	 // receive data byte(s)
			if (w==SYNC_SERVO)
			{	// read rest of data
				if (serial->receiveLen()>=4)
				{
					serial->receive((uint8_t *)&s0, 2);
					serial->receive((uint8_t *)&s1, 2);

					//cprintf("servo %d %d\n", s0, s1);
					rcs_setPos(0, s0);
					rcs_setPos(1, s1);

					g_state = 0;
				}
			}
			else if (w==SYNC_CAM_BRIGHTNESS)
			{
				if(serial->receive(&a, 1))
				{
					cam_setBrightness(a);
					g_state = 0;
				}
			}
			else if (w==SYNC_SET_LED)
			{
				if (serial->receiveLen()>=3)
				{
					uint8_t r, g, b;
					serial->receive(&r, 1);
					serial->receive(&g, 1);
					serial->receive(&b, 1);

					led_setRGB(r, g, b);
					//cprintf("%x %x %x\n", r, g ,b);

					g_ledSet = true; // it will stay true until the next power cycle
					g_state = 0;
				}
			}
			else 
				g_state = 1; // try another word, but read only a byte
			break;

		default:
			g_state = 0; // try another whole word
			break;
		}
	}
}

int blobsLoop()
{
#if 1
	BlobA *blobs;
	BlobB *ccBlobs;
	uint32_t numBlobs, numCCBlobs;
	static uint32_t drop = 0;

	// create blobs
	if (g_blobs->blobify()<0)
	{
		DBG("drop %d\n", drop++);
		return 0;
	}
	// handle received data immediately
#ifdef LEGO
	lego_handleRecv();
#else
	handleRecv();
#endif

	// send blobs
	g_blobs->getBlobs(&blobs, &numBlobs, &ccBlobs, &numCCBlobs);
	cc_sendBlobs(g_chirpUsb, blobs, numBlobs, ccBlobs, numCCBlobs);

	ser_getSerial()->update();

	// if user isn't controlling LED, set it here, according to biggest detected object
	if (!g_ledSet)
		cc_setLED();
	
	// deal with any latent received data until the next frame comes in
	while(!g_qqueue->queued())
#ifdef LEGO
		lego_handleRecv();
#else
		handleRecv();
#endif

#endif
#if 0
	Qval qval;
	int j = 0;
	static int i = 0;
	while(1)
	{
		if (g_qqueue->dequeue(&qval))
		{
			j++;
			if (qval.m_col>=0xfffe)
			{
				cprintf("%d: %d %x\n", i++, j, qval.m_col);
				break;
			}
		}
	}
#endif
#if 0
	BlobA *blobs;
	BlobB *ccBlobs;
	uint32_t numBlobs, numCCBlobs;
	static uint32_t drop = 0;

	// create blobs
	if (g_blobs->blobify()<0)
	{
		DBG("drop %d\n", drop++);
		return 0;
	}
	g_blobs->getBlobs(&blobs, &numBlobs, &ccBlobs, &numCCBlobs);
	cc_sendBlobs(g_chirpUsb, blobs, numBlobs, ccBlobs, numCCBlobs);

#endif

	return 0;
}
