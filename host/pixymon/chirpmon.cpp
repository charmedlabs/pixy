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

#include <QDebug>
#include <QMutexLocker>
#include <stdexcept>
#include "chirpmon.h"
#include "interpreter.h"

ChirpMon::ChirpMon(Interpreter *interpreter, USBLink *link)
{
    m_hinterested = true;
    m_client = true;
    m_interpreter = interpreter;

    if (setLink(link)<0)
        throw std::runtime_error("Unable to connect to device.");
}

ChirpMon::~ChirpMon()
{
}


int ChirpMon::serviceChirp()
{
    uint8_t type;
    ChirpProc recvProc;
    void *args[CRP_MAX_ARGS+1];
    int res;

    while(1)
    {
        if ((res=recvChirp(&type, &recvProc, args, true))<0)
            return res;
        handleChirp(type, recvProc, (const void **)args);
        if (type&CRP_RESPONSE)
            break;
    }
    return 0;
}


int ChirpMon::handleChirp(uint8_t type, ChirpProc proc, const void *args[])
{
    if (type==CRP_RESPONSE)
    {
        m_interpreter->handleResponse(args);
        return 0;
    }

    return Chirp::handleChirp(type, proc, args);
}

void ChirpMon::handleXdata(const void *data[])
{
    m_interpreter->handleData(data);
}

int ChirpMon::sendChirp(uint8_t type, ChirpProc proc)
{   // this is only called when we call call()
    int res;

    // if we're programming (defining the program), put all the calls in m_program
    // otherwise pass the call the Chirp::sendChirp() so it gets sent out.
    // todo: save the call and use the chirp thread to send (so send and receive are handled by
    // same thread. not sure how important that is...)
    if (m_interpreter->m_programming && !(type&CRP_INTRINSIC))
    {
        // put on queue
        // only copy data (not header).  Header hasn't been written to buffer yet.
        m_interpreter->addProgram(ChirpCallData(type, proc, m_buf+m_headerLen, m_len));
        return 0;
    }

    res = Chirp::sendChirp(type, proc);

    return res;
}

int ChirpMon::execute(const ChirpCallData &data)
{
    int res;

    // copy into chirp buffer-- remember to skip the header space
    memcpy(m_buf+m_headerLen, data.m_buf, data.m_len);
    m_len = data.m_len;
    if ((res=Chirp::sendChirp(data.m_type, data.m_proc))<0)
        return res;
    if ((res=serviceChirp())<0)
        return res;

    return 0;
}

