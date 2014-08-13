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

#include <QDebug>
#include "usblink.h"
#include "sleeper.h"
#include "pixydefs.h"



USBLink::USBLink()
{
    m_handle = 0;
    m_context = 0;
    m_blockSize = 64;
    m_flags = LINK_FLAG_ERROR_CORRECTED;
}

USBLink::~USBLink()
{
    if (m_handle)
        libusb_close(m_handle);
    if (m_context)
        libusb_exit(m_context);
}

int USBLink::open()
{
    libusb_init(&m_context);

    m_handle = libusb_open_device_with_vid_pid(m_context, PIXY_VID, PIXY_DID);
    if (m_handle==NULL)
        return -1;
#ifdef __MACOS__
    libusb_reset_device(m_handle);
    Sleeper::msleep(100);
#endif
    if (libusb_set_configuration(m_handle, 1)<0)
    {
        libusb_close(m_handle);
        m_handle = 0;
        return -1;
    }
    if (libusb_claim_interface(m_handle, 1)<0)
    {
        libusb_close(m_handle);
        m_handle = 0;
        return -1;
    }
#ifdef __LINUX__
    libusb_reset_device(m_handle);
#endif
    return 0;
}



int USBLink::send(const uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
    int res, transferred;

    if (timeoutMs==0) // 0 equals infinity
        timeoutMs = 10;

    if ((res=libusb_bulk_transfer(m_handle, 0x02, (unsigned char *)data, len, &transferred, timeoutMs))<0)
    {
#ifdef __MACOS__
        libusb_clear_halt(m_handle, 0x02);
#endif
        qDebug("libusb_bulk_write %d", res);
        return res;
    }
    return transferred;
}

int USBLink::receive(uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
    int res, transferred;

    if (timeoutMs==0) // 0 equals infinity
        timeoutMs = 50;

    // Note: if this call is taking more time than than expected, check to see if we're connected as USB 2.0.  Bad USB cables can
    // cause us to revert to a 1.0 connection.
    if ((res=libusb_bulk_transfer(m_handle, 0x82, (unsigned char *)data, len, &transferred, timeoutMs))<0)
    {
#ifdef __MACOS__
        libusb_clear_halt(m_handle, 0x82);
#endif
        qDebug("libusb_bulk_read %d", res);
        return res;
    }
    return transferred;
}

void USBLink::setTimer()
{
    m_time.start();
}

uint32_t USBLink::getTimer()
{
    return m_time.elapsed();
}



