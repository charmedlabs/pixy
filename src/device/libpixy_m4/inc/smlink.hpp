#ifndef SMLINK_HPP
#define SMLINK_HPP

#include "pixyvals.h"
#include "link.h"

// status
#define SM_STATUS_DATA_AVAIL   0x01

struct SmMap
{
    volatile uint16_t recvStatus;
    volatile uint16_t sendStatus;

    volatile uint8_t buf[MEM_SM_BUFSIZE];
};

#define SM_OBJECT       ((SmMap *)MEM_SM_LOC)


class SMLink : public Link
{
public:
    SMLink();
    ~SMLink();
    virtual int send(const uint8_t *data, uint32_t len, uint16_t timeoutMs);
    virtual int receive(uint8_t *data, uint32_t len, uint16_t timeoutMs);
    virtual void setTimer();
    virtual uint32_t getTimer();
    virtual uint32_t getFlags(uint8_t index=LINK_FLAG_INDEX_FLAGS);

private:
    uint32_t m_timer;
};

#endif
