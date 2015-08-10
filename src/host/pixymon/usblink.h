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

#ifndef _USBLINK_H
#define _USBLINK_H

#include <link.h>
#include <QTime>
#include "libusb.h"

class USBLink : public Link
{
public:
    USBLink();
    virtual ~USBLink();

    int open();
    void close();
    virtual int send(const uint8_t *data, uint32_t len, uint16_t timeoutMs);
    virtual int receive(uint8_t *data, uint32_t len, uint16_t timeoutMs);
    virtual void setTimer();
    virtual uint32_t getTimer();

private:
    int openDevice();
    libusb_context *m_context;
    libusb_device_handle *m_handle;
    QTime m_time;
};
#endif

