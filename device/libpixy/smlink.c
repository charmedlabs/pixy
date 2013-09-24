#include "lpc43xx.h"
#include "chirp.h"
#include "smlink.h"


uint32_t linkGetFlags(uint8_t index)
{
	if (index==LINK_FLAG_INDEX_SHARED_MEMORY_LOCATION)
		return (uint32_t)SM_OBJECT->buf;
	else if (index==LINK_FLAG_INDEX_SHARED_MEMORY_SIZE)
		return SM_BUFSIZE;
	else
		return 0;
}

int linkSend(const uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
	uint32_t time, start, timeout = timeoutMs * CLKFREQ_MS;

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

int linkReceive(uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
	uint32_t time, start, timeout = timeoutMs * CLKFREQ_MS;

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
