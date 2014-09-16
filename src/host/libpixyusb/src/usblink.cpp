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

#include <unistd.h>
#include <stdio.h>
#include "usblink.h"
#include "pixy.h"
#include "utils/timer.hpp"

USBLink::USBLink()
{
  m_handle = 0;
  m_context = 0;
  m_blockSize = 64;
  m_flags = LINK_FLAG_ERROR_CORRECTED;
}

USBLink::~USBLink()
{
  fflush(stdout);
    if (m_handle)
        libusb_close(m_handle);
    if (m_context)
        libusb_exit(m_context);
}

int USBLink::open()
{
    int set_config_return_value;
    int claim_interface_return_value;

    libusb_init(&m_context);

    m_handle = libusb_open_device_with_vid_pid(m_context, PIXY_VID, PIXY_DID);
    if (m_handle==NULL)
        return PIXY_ERROR_USB_NOT_FOUND;
#ifdef __MACOS__
    const unsigned int MILLISECONDS_TO_SLEEP = 100;
    libusb_reset_device(m_handle);
    usleep(MILLISECONDS_TO_SLEEP * 1000);
#endif
    set_config_return_value = libusb_set_configuration(m_handle, 1);
    if (set_config_return_value < 0)
    {
        libusb_close(m_handle);
        m_handle = 0;
        return set_config_return_value;
    }

    claim_interface_return_value = libusb_claim_interface(m_handle, 1);
    if (claim_interface_return_value < 0)
    {
        libusb_close(m_handle);
        m_handle = 0;
        return claim_interface_return_value;
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
        printf("libusb_bulk_write %d\n", res);
        return res;
    }
    return transferred;
}

int USBLink::receive(uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
    int res, transferred;

    if (timeoutMs==0) // 0 equals infinity
        timeoutMs = 50;

    if ((res=libusb_bulk_transfer(m_handle, 0x82, (unsigned char *)data, len, &transferred, timeoutMs))<0)
    {
#ifdef __MACOS__
        libusb_clear_halt(m_handle, 0x82);
#endif
        printf("libusb_bulk_read %d\n", res);
        return res;
    }
    return transferred;
}

void USBLink::setTimer()
{
  timer_.reset();
}

uint32_t USBLink::getTimer()
{
  return timer_.elapsed();
}

