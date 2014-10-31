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
