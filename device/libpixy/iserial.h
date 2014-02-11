#ifndef _ISERIAL_H
#define _ISERIAL_H

#include <inttypes.h>

typedef uint32_t (*SerialCallback)(uint8_t *data, uint32_t len); 

// circular queue, for receiving data
struct ReceiveQ
{
	uint8_t *m_buf;
	uint32_t m_read;
	uint32_t m_write;
	uint32_t m_produced;
	uint32_t m_consumed;
};


// linear queue, to buffer a chunk and dispense it out
struct TransmitQ
{
	uint8_t *m_buf;
	uint32_t m_read;
	uint32_t m_len;
};

// pure virtual interface to a serial device
class Iserial
{
public:
	virtual int open() = 0;
	virtual int close() = 0;
	virtual int receive(uint8_t *buf, uint32_t len) = 0;
	virtual int update() = 0;
};

#endif

