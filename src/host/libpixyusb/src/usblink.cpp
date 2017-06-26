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
#include "debuglog.h"


USBLink::USBLink()
{
  m_handle = 0;
  m_context = 0;
  m_blockSize = 64;
  m_flags = LINK_FLAG_ERROR_CORRECTED;
}

USBLink::~USBLink()
{
  if (m_handle) {
    libusb_close(m_handle);
    log("pixydebug:  libusb_close()\n");
  }
  if (m_context) {
    libusb_exit(m_context);
    log("pixydebug:  libusb_exit()\n");
  }
}

int USBLink::open(int ord)
{
  int i, ipixy = 0, count = 0;
  libusb_device *device;
  libusb_device_descriptor desc;
  libusb_device **list = NULL;
  int return_value = 0;
#ifdef __MACOS__
  const unsigned int MILLISECONDS_TO_SLEEP = 100;
#endif

  log("pixydebug: USBLink::open()\n");

  if (m_context == NULL) {
    return_value = libusb_init(&m_context);
    log("pixydebug:  libusb_init() = %d\n", return_value);
  }

  if (return_value == 0)
    count = libusb_get_device_list(m_context, &list);

  for (i=0; i < count; i++)
  {
    device = list[i];
    libusb_get_device_descriptor(device, &desc);

    if (desc.idVendor != PIXY_VID || desc.idProduct != PIXY_PID)
      continue;

    // Release any open handle we currently have
    if ( m_handle != NULL )
      { libusb_close(m_handle); m_handle = NULL; }

    return_value = libusb_open(device, &m_handle);
    log("pixydebug:  libusb_open() = %d\n", return_value);

#ifdef __MACOS__
    if (return_value == 0)
    {
      return_value = libusb_reset_device(m_handle);
      log("pixydebug:  libusb_reset_device() = %d\n", return_value);
      usleep(MILLISECONDS_TO_SLEEP * 1000);
    }
#endif
    if (return_value == 0)
    {
      return_value = libusb_set_configuration(m_handle, 1);
      log("pixydebug:  libusb_set_configuration() = %d\n", return_value);
    }
    if (return_value == 0)
    {
      return_value = libusb_claim_interface(m_handle, 1);
      log("pixydebug:  libusb_claim_interface() = %d\n", return_value);
    }
#ifdef __LINUX__
    if (return_value == 0)
    {
      return_value = libusb_reset_device(m_handle);
      log("pixydebug:  libusb_reset_device() = %d\n", return_value);
    }
#endif
    if (return_value == 0 && ipixy == ord) // success!
      break;

    // We suffered an error setting up the device; if it's just busy continue
    // otherwise break and return the error code
    ipixy++;
    if (return_value != LIBUSB_ERROR_BUSY)
      break;
  }
  if (list != NULL)
    libusb_free_device_list(list, 1);

  if (i == count && return_value == 0)
    return_value = PIXY_ERROR_USB_NO_DEVICE;
  // if count < 0 then get_device_list returned a libusb_error
  else if (count < 0)
    return_value = count;

  log("pixydebug: USBLink::open() returned %d\n", return_value);

  return return_value;
}

int USBLink::send(const uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
    int res, transferred;

    log("pixydebug: USBLink::send()\n");

    if (timeoutMs==0) // 0 equals infinity
        timeoutMs = 10;

    if ((res=libusb_bulk_transfer(m_handle, 0x02, (unsigned char *)data, len, &transferred, timeoutMs))<0)
    {
        log("pixydebug:  libusb_bulk_transfer() = %d\n", res);
#ifdef __MACOS__
        libusb_clear_halt(m_handle, 0x02);
#endif
        log("pixydebug: USBLink::send() returned %d\n", res);
        return res;
    }

    log("pixydebug: USBLink::send() returned %d\n", transferred);
    return transferred;
}

int USBLink::receive(uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
    int res, transferred;

    log("pixydebug: USBLink::receive()\n");

    if (timeoutMs==0) // 0 equals infinity
        timeoutMs = 50;

    if ((res=libusb_bulk_transfer(m_handle, 0x82, (unsigned char *)data, len, &transferred, timeoutMs))<0)
    {
        log("pixydebug:  libusb_bulk_transfer() = %d\n", res);
#ifdef __MACOS__
        libusb_clear_halt(m_handle, 0x82);
#endif
        return res;
    }

    log("pixydebug:  libusb_bulk_transfer(%d bytes) = %d\n", len, res);
    log("pixydebug: USBLink::receive() returned %d (bytes transferred)\n", transferred);
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

