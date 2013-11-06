#ifndef CONNECTEVENT_H
#define CONNECTEVENT_H

#include <QThread>
#include <QMutex>
#include "libusb.h"

class MainWindow;

class ConnectEvent : public QThread
{
    Q_OBJECT

public:
    enum Device {NONE, PIXY, PIXY_DFU};

    ConnectEvent(MainWindow *main, unsigned int sleep=0);
    Device getConnected();
    ~ConnectEvent();


signals:
    void connected(ConnectEvent::Device device, bool state);

protected:
    virtual void run();

private:
    libusb_context *m_context;
    MainWindow *m_main;
    unsigned int m_sleep;
    QMutex m_mutex;
    bool m_run;
};

#endif // CONNECTEVENT_H
