#include "disconnectevent.h"
#include "interpreter.h"

DisconnectEvent::DisconnectEvent(Interpreter *interpreter)
{
    m_interpreter = interpreter;
    m_run = true;
    start();
}

DisconnectEvent::~DisconnectEvent()
{
    m_run = false;
    wait();
}

// this thread simply "pings" pixy periodically checking for an error
// It's simple, detects hangs on Pixy and is portable between OS's
void DisconnectEvent::run()
{
    int res;
    while(m_run)
    {
        res = m_interpreter->getRunning();
        if (res<0)
        {
            m_interpreter->emit connected(PIXY, false);
            break;
        }
        msleep(DE_PERIOD);
    }
}


