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

#include "lpc43xx.h"
#include "misc.h"
#include "smlink.hpp"

#define CLKRATE   204000000
#define CLKRATEMS (CLKRATE/1000)

SMLink::SMLink()
{
	m_flags = LINK_FLAG_ERROR_CORRECTED | LINK_FLAG_SHARED_MEM;

	SM_OBJECT->sendStatus = 0;
	SM_OBJECT->recvStatus = 0;
}

SMLink::~SMLink()
{
}

int SMLink::send(const uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
	uint32_t time, start, timeout = timeoutMs * CLKRATEMS;

	start = LPC_TIMER1->TC;
	// wait for data to go out
	while(SM_OBJECT->sendStatus==SM_STATUS_DATA_AVAIL)
	{
		time = LPC_TIMER1->TC; 
		if ((uint32_t)(time-start) > timeout)
			return -1;
	}
	// set status to indicate data is avail
	SM_OBJECT->sendStatus = SM_STATUS_DATA_AVAIL;	
	return len;
}

int SMLink::receive(uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
	uint32_t time, start, timeout = timeoutMs * CLKRATEMS;

	start = LPC_TIMER1->TC;
	// wait for data to come in
	while(!(SM_OBJECT->recvStatus==SM_STATUS_DATA_AVAIL))
	{
		time = LPC_TIMER1->TC; 
		if ((uint32_t)(time-start) > timeout)
			return -1;
	}
	// set status to indicate data has been read	
	SM_OBJECT->recvStatus = 0;	

	return len;
}


void SMLink::setTimer()
{
	::setTimer(&m_timer);
}

uint32_t SMLink::getTimer()
{
	return ::getTimer(m_timer);
}

uint32_t SMLink::getFlags(uint8_t index)
{
	if (index==LINK_FLAG_INDEX_SHARED_MEMORY_LOCATION)
		return (uint32_t)SM_OBJECT->buf;
	else if (index==LINK_FLAG_INDEX_SHARED_MEMORY_SIZE)
		return SM_BUFSIZE;
	else 
		return Link::getFlags(index);
}

