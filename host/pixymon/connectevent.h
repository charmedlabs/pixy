#ifndef CONNECTEVENT_H
#define CONNECTEVENT_H

#include <QThread>

class MainWindow;

class ConnectEvent : public QThread
{
    Q_OBJECT

public:
    ConnectEvent(MainWindow *main);
    ~ConnectEvent();

    enum Device {PIXY, PIXY_DFU};

signals:
    void connected(ConnectEvent::Device device, bool state);

protected:
    virtual void run();

private:
    MainWindow *m_main;
    bool m_run;
};

#endif // CONNECTEVENT_H
