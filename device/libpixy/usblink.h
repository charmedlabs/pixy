#ifndef _USBLINK_H
#define _USBLINK_H

#include "link.h"

class USBLink : public Link
{
public:
	USBLink();
	~USBLink();
    virtual int send(const uint8_t *data, uint32_t len, uint16_t timeoutMs);
    virtual int receive(uint8_t *data, uint32_t len, uint16_t timeoutMs);
    virtual void setTimer();
    virtual uint32_t getTimer();

private:
	uint32_t m_timer;
};
#endif

