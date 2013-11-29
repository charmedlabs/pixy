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
    libusb_context *m_context;
    MainWindow *m_main;
    unsigned int m_sleep;
    QMutex m_mutex;
    bool m_run;
};

#endif // CONNECTEVENT_H
