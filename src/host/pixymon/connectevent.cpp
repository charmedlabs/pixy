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

#include "connectevent.h"
#include "libusb.h"
#include "pixydefs.h"
#include "usblink.h"
#include "mainwindow.h"

ConnectEvent::ConnectEvent(MainWindow *main, unsigned int sleep)
{
    m_main = main;
    m_sleep = sleep;
    m_run = true;
    m_state = -1;
    m_dev = NONE;

    libusb_init(&m_context);
    start();
}

ConnectEvent::~ConnectEvent()
{
    m_run = false;
    wait();
    libusb_exit(m_context);
}

Device ConnectEvent::getConnected()
{
    Device res = NONE;
    libusb_device_handle *handle = 0;
    USBLink *plink;

    m_mutex.lock();
    plink = new USBLink();
    if (plink->open()==0)
        res = PIXY;
    else
    {
        handle = libusb_open_device_with_vid_pid(m_context, PIXY_DFU_VID, PIXY_DFU_PID);
        if (handle)
            res = PIXY_DFU;
    }
    delete plink;
    if (handle)
        libusb_close(handle);
    m_mutex.unlock();

    return res;
}

void ConnectEvent::emitConnected(Device dev, bool state)
{
    // this makes sure we only send connect events upon changes
    // which also prevents late events from coming in after we destroy this class instance.
    if (dev!=m_dev || state!=m_state)
    {
        m_dev = dev;
        m_state = state;

        emit connected(m_dev, m_state);
    }
}

// this polling loop is much more portable between OSs than detecting actual connect/disconnect events
void ConnectEvent::run()
{
    Device dev;

    connect(this, SIGNAL(connected(Device,bool)), m_main, SLOT(handleConnected(Device,bool)));

    // sleep a while (so we can wait for other devices to be de-registered)
    msleep(m_sleep);
    while(m_run)
    {
        dev = getConnected();
        msleep(1000);
        if (dev!=NONE)
            emitConnected(dev, true);
        else
            emitConnected(dev, false);
    }
}

