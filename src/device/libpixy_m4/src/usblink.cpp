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
#include "lpc_types.h"
#include "pixyvals.h"
#include "usb.h"
#include "usbcfg.h"
#include "usblink.h"
#include "usbuser.h"
#include "usbhw.h"
#include "lpc43xx.h"
#include "misc.h"

#define GBUF_SIZE 64

uint8_t g_buf[GBUF_SIZE];
uint32_t g_bufUsed = 0;

USBLink::USBLink()
{
	m_flags = LINK_FLAG_ERROR_CORRECTED;
}

USBLink::~USBLink()
{
}

int USBLink::send(const uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
	uint32_t time, start, timeout = timeoutMs * CLKFREQ_MS;

 	if (!USB_handleState())
	{
		g_sendComplete = 0;
		return -1;
	}

	USB_Send(data, len);
	while(1)
	{
		start = g_sendTimerStart; // avoid race condition with usb interrupt routine-- sample here 
		time = LPC_TIMER1->TC; // time is guaranteed to be more recent than start
		if ((uint32_t)(time-start) > timeout || g_sendComplete)
			break;
	}
  	if (g_sendComplete)
		return len;
	else
	{
		USB_SendReset();
		return LINK_RESULT_ERROR_SEND_TIMEOUT;
	}
}

int USBLink::receive(uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
	uint32_t time, start, timeout = timeoutMs * CLKFREQ_MS;

 	if (!USB_handleState())
	{
		g_bufUsed = 0;
		g_recvComplete = 0;
		return -1;
	}

	if (timeout==0) // this is special case... 
	{
		if ((len>GBUF_SIZE) || ((g_bufUsed!=0) && (g_bufUsed!=len)))
			return LINK_RESULT_ERROR;

		if (g_bufUsed==0)
		{
			g_bufUsed = len;
			// register	receive buffer
			USB_Recv(g_buf, g_bufUsed);
		}
		else if (g_recvComplete) // if it has come in, then copy
		{
			memcpy((void *)data, (void *)g_buf, len);
			g_bufUsed = 0;
			return len;
		}
		return LINK_RESULT_ERROR_RECV_TIMEOUT; // if the data isn't there, return a timeout error -- this is consistent with libusb
	}
	else
	{
		if (g_bufUsed!=0) // if we have a receive pending, reset
			USB_RecvReset();			
		USB_Recv(data, len);
	}

   	// wait
	while(1)
	{
		start = g_recvTimerStart; // avoid race condition with usb interrupt routine-- sample here 
		time = LPC_TIMER1->TC; // time is guaranteed to be more recent than start
		if ((uint32_t)(time-start) > timeout || g_recvComplete)
			break;
	}
  	if (g_recvComplete)
		return len;
	else 
	{
		USB_RecvReset();
		return LINK_RESULT_ERROR_RECV_TIMEOUT;
	}
}


void USBLink::setTimer()
{
	::setTimer(&m_timer);
}

uint32_t USBLink::getTimer()
{
	return ::getTimer(m_timer);
}
