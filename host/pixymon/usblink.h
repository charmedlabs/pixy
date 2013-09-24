#ifndef _USBLINK_H
#define _USBLINK_H

#include "link.h"
#include "lusb0_usb.h"

class USBLink : public Link
{
public:
    USBLink();
    ~USBLink();

    int open();
    virtual int send(const uint8_t *data, uint32_t len, uint16_t timeoutMs);
    virtual int receive(uint8_t *data, uint32_t len, uint16_t timeoutMs);

private:
   usb_dev_handle *m_dev;
};
#endif

