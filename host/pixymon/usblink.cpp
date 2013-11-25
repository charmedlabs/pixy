#include <QDebug>
#include "usblink.h"
#include "pixy.h"

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
    if (libusb_set_configuration(m_handle, 1)<0)
    {
        libusb_close(m_handle);
        return -1;
    }
    if (libusb_claim_interface(m_handle, 1)<0)
    {
        libusb_close(m_handle);
        return -1;
    }

    return 0;
}



int USBLink::send(const uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
    int res, transferred;

    if (timeoutMs==0) // 0 equals infinity
        timeoutMs = 10;

    if ((res=libusb_bulk_transfer(m_handle, 0x02, (unsigned char *)data, len, &transferred, timeoutMs))<0)
    {
        qDebug() << "libusb_bulk_write " << res;
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
        qDebug() << "libusb_bulk_read " << res;
        return res;
    }
    return transferred;
}




