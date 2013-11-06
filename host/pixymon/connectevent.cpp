#include <QMetaType>
#include "libusb.h"
#include "pixy.h"
#include "mainwindow.h"

ConnectEvent::ConnectEvent(MainWindow *main, unsigned int sleep)
{
    qRegisterMetaType<ConnectEvent::Device>("ConnectEvent::Device");
    m_main = main;
    m_sleep = sleep;
    m_run = true;
    libusb_init(&m_context);
    start();
}

ConnectEvent::~ConnectEvent()
{
    m_run = false;
    wait();
    libusb_exit(m_context);
}

ConnectEvent::Device ConnectEvent::getConnected()
{
    Device res = NONE;
    libusb_device_handle *handle = 0;

    m_mutex.lock();
    handle = libusb_open_device_with_vid_pid(m_context, PIXY_VID, PIXY_DID);
    if (handle)
        res = PIXY;
    else
    {
        handle = libusb_open_device_with_vid_pid(m_context, PIXY_DFU_VID, PIXY_DFU_DID);
        if (handle)
            res = PIXY_DFU;
    }
    if (handle)
        libusb_close(handle);
    m_mutex.unlock();

    return res;
}

// this polling loop is much more portable between OSs than detecting actual connect/disconnect events
void ConnectEvent::run()
{
    Device dev;

    connect(this, SIGNAL(connected(ConnectEvent::Device,bool)), m_main, SLOT(handleConnected(ConnectEvent::Device,bool)));

    // sleep a while (so we can wait for other devices to be de-registered)
    msleep(m_sleep);
    while(m_run)
    {
        dev = getConnected();
        if (dev!=NONE)
        {
            emit connected(dev, true);
            break;
        }
        msleep(1000);
    }
}

