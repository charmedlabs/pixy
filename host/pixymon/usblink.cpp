#include <QDebug>
#include "usblink.h"

USBLink::USBLink()
{
    m_dev = 0;
    m_flags = LINK_FLAG_ERROR_CORRECTED;
}

USBLink::~USBLink()
{
    if (m_dev)
        usb_close(m_dev);
}

int USBLink::open()
{
    struct usb_bus *bus, *busses;
    int conf;

    usb_init();
    usb_find_busses();
    usb_find_devices();

    busses = usb_get_busses();

    for (bus=busses; bus; bus = bus->next)
    {
        struct usb_device *dev;
        for (dev=bus->devices; dev; dev = dev->next)
        {
            if (dev->descriptor.idVendor==0xb1ac && dev->descriptor.idProduct==0xf000)
            {
                for (conf=0; conf<dev->descriptor.bNumConfigurations; conf++)
                {
                    m_dev = usb_open(dev);

                    if (usb_set_configuration(m_dev, 1)<0)
                    {
                        usb_close(m_dev);
                        return -1;
                    }
                    if (usb_claim_interface(m_dev, 1)<0)
                    {
                        usb_close(m_dev);
                        return -1;
                    }
                    // clear receive buffer-- it seems that the libusb driver will buffer and send stale data
                    // unless you reset
                    usb_resetep(m_dev, 0x82);
                    usb_resetep(m_dev, 0x02);

                    return 0;
                }
            }
        }
    }
    return -1;
}

int USBLink::send(const uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
    int res;

    if ((res=usb_bulk_write(m_dev, 0x02, (char *)data, len, timeoutMs))<0)
    {
        qDebug() << "usb_bulk_write " << res;
        //usb_reset(m_dev);
        //usb_close(m_dev);
        //open();
        return res;
    }
    return len;
}

int USBLink::receive(uint8_t *data, uint32_t len, uint16_t timeoutMs)
{
    int res;

    if ((res=usb_bulk_read(m_dev, 0x82, (char *)data, len, timeoutMs))<0)
    {
        qDebug() << "usb_bulk_read " << res;
        return res;
    }
    return len;
}




