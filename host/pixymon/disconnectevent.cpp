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

// it's probably best to just have a single thread deal with chirp.
// just poll getRunning every n chrip services.
// this simplifies things....
// we can get rid of this class, and make the run/stop sensing more robust.
// And we can make the button initiate the run state when setting signatures (which we can't do right now).
// And it's sort of weird that when we're stopped, we are rendering xdata through this thread, sort of
// inadvertently.... to be tackled later
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


