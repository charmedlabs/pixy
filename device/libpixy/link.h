#ifndef LINK_H
#define LINK_H

#include <stdint.h>

// flags
#define LINK_FLAG_SHARED_MEM                            0x01
#define	LINK_FLAG_ERROR_CORRECTED                       0x02

// result codes
#define LINK_RESULT_OK                                  0
#define LINK_RESULT_ERROR                               -100
#define LINK_RESULT_ERROR_RECV_TIMEOUT                  -101
#define LINK_RESULT_ERROR_SEND_TIMEOUT                  -102

// link flag index
#define LINK_FLAG_INDEX_FLAGS                           0x00
#define LINK_FLAG_INDEX_SHARED_MEMORY_LOCATION          0x01
#define LINK_FLAG_INDEX_SHARED_MEMORY_SIZE              0x02


class Link
{
public:
    Link()
    {
        m_flags = 0;
        m_blockSize = 0;
    }
    ~Link()
    {
    }

    // the timeoutMs is a timeout value in milliseconds.  The timeout timer should expire
    // when the data channel has been continuously idle for the specified amount of time
    // not the summation of the idle times.
    virtual int send(const uint8_t *data, uint32_t len, uint16_t timeoutMs) = 0;
    virtual int receive(uint8_t *data, uint32_t len, uint16_t timeoutMs) = 0;
    virtual uint32_t getFlags(uint8_t index=LINK_FLAG_INDEX_FLAGS)
    {
        if (index==LINK_FLAG_INDEX_FLAGS)
            return m_flags;
        else
            return 0;
    }
    virtual uint32_t blockSize()
    {
        return m_blockSize;
    }
    virtual int getBuffer(uint8_t **buf, uint32_t *len)
    {
        return LINK_RESULT_ERROR;
    }

protected:
    uint32_t m_flags;
    uint32_t m_blockSize;
};

#endif // LINK_H
