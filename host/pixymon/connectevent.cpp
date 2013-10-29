#include <QMetaType>
#include "libusb.h"
#include "connectevent.h"
#include "pixy.h"
#include "mainwindow.h"

ConnectEvent::ConnectEvent(MainWindow *main)
{
    qRegisterMetaType<ConnectEvent::Device>("ConnectEvent::Device");
    m_main = main;
    m_run = true;
    start();
}

ConnectEvent::~ConnectEvent()
{
    m_run = false;
    wait();
}


void ConnectEvent::run()
{
    libusb_context *context;
    libusb_device_handle *handle = 0;

    connect(this, SIGNAL(connected(ConnectEvent::Device,bool)), m_main, SLOT(handleConnected(ConnectEvent::Device,bool)));

    libusb_init(&context);

    while(m_run)
    {
        handle = libusb_open_device_with_vid_pid(context, PIXY_VID, PIXY_DID);
        if (handle)
        {
            emit connected(PIXY, true);
            break;
        }
        handle = libusb_open_device_with_vid_pid(context, PIXY_DFU_VID, PIXY_DFU_DID);
        if (handle)
        {
            emit connected(PIXY_DFU, true);
            break;
        }
        msleep(1000);
    }

    if (handle)
        libusb_close(handle);
    libusb_exit(context);
}

