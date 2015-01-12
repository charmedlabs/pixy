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

#ifndef CHIRPTHREAD_H
#define CHIRPTHREAD_H

#include <chirp.hpp>

class Interpreter;
class USBLink;

struct ChirpCallData
{
    ChirpCallData(uint8_t type, ChirpProc proc, uint8_t *buf, uint32_t len)
    {
        m_type = type;
        m_proc = proc;
        m_buf = new uint8_t[len];
        memcpy(m_buf, buf, len);
        m_len = len;
    }
    // no destructor -- need to free the memory explicitly
    // (freeing mem in destructor is not the correct way to do it in this case unless we
    // also write a copy constructor.)

    uint8_t m_type;
    ChirpProc m_proc;
    uint8_t *m_buf;
    uint32_t m_len;
};

class ChirpMon : public Chirp
{
public:
    ChirpMon(Interpreter *interpreter, USBLink *link);
    virtual ~ChirpMon();

    int serviceChirp();

    friend class Interpreter;

protected:
    virtual int handleChirp(uint8_t type, ChirpProc proc, const void *args[]); // null pointer terminates
    virtual void handleXdata(const void *data[]);
    virtual int sendChirp(uint8_t type, ChirpProc proc);

private:
    int execute(const ChirpCallData &data);

    Interpreter *m_interpreter;
};

#endif // CHIRPTHREAD_H
