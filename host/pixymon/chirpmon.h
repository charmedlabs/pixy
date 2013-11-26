#ifndef CHIRPTHREAD_H
#define CHIRPTHREAD_H

#include "../../device/libpixy/chirp.hpp"

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
    virtual int handleChirp(uint8_t type, ChirpProc proc, void *args[]); // null pointer terminates
    virtual void handleXdata(void *data[]);
    virtual int sendChirp(uint8_t type, ChirpProc proc);

private:
    int execute(const ChirpCallData &data);

    Interpreter *m_interpreter;
};

#endif // CHIRPTHREAD_H
