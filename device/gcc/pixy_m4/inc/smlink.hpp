#ifndef SMLINK_HPP
#define SMLINK_HPP

#include "pixyvals.h"
#include "link.h"

#define SM_LOC                 (SRAM4_LOC+0x3000)
#define SM_SIZE                (SRAM4_SIZE-0x3000)
#define SM_BUFSIZE             (SM_SIZE-4)

// status
#define SM_STATUS_DATA_AVAIL   0x01

struct SmMap
{
	volatile uint16_t recvStatus;
	volatile uint16_t sendStatus;

	volatile uint8_t buf[SM_BUFSIZE];
};

#define SM_OBJECT       ((SmMap *)SM_LOC)


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
