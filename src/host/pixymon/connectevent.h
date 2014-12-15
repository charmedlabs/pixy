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

#ifndef CONNECTEVENT_H
#define CONNECTEVENT_H

#include <QThread>
#include <QMutex>
#include "libusb.h"
#include "mainwindow.h"


class ConnectEvent : public QThread
{
    Q_OBJECT

public:

    ConnectEvent(MainWindow *main, unsigned int sleep=0);
    Device getConnected();
    ~ConnectEvent();

signals:
    void connected(Device device, bool state);

protected:
    virtual void run();

private:
    void emitConnected(Device dev, bool state);

    libusb_context *m_context;
    MainWindow *m_main;
    unsigned int m_sleep;
    QMutex m_mutex;
    bool m_run;

    Device m_dev;
    bool m_state;
};

#endif // CONNECTEVENT_H
