#ifndef DISCONNECTEVENT_H
#define DISCONNECTEVENT_H

#include <QThread>
#include "mainwindow.h"

#define DE_PERIOD              500 // milliseconds

class Interpreter;

class DisconnectEvent : public QThread
{
    Q_OBJECT

public:
    DisconnectEvent(Interpreter *interpreter);
    ~DisconnectEvent();

signals:
    void connected(Device device, bool state);

protected:
    virtual void run();

private:
    Interpreter *m_interpreter;
    bool m_run;
};

#endif // DISCONNECTEVENT_H
