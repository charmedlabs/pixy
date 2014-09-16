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

#ifndef _ISERIAL_H
#define _ISERIAL_H

#include <inttypes.h>

typedef uint32_t (*SerialCallback)(uint8_t *data, uint32_t len); 

// circular queue, for receiving data
template <class BufType> class ReceiveQ
{
public:
	ReceiveQ(uint32_t size)
	{
		m_size = size;
		m_buf = new BufType[m_size];
		m_read = 0;
		m_write = 0;
		m_produced = 0;
		m_consumed = 0;
	}

   	~ReceiveQ()
	{
		delete [] m_buf;
	}

	inline int32_t receiveLen()
	{
		return m_produced - m_consumed;
	}

	inline int32_t freeLen()
	{
		return m_size - receiveLen();
	}
	 
	inline int read(BufType *data)
	{
		if (receiveLen()<=0)
			return 0;
		*data = m_buf[m_read++];
		m_consumed++;

		if (m_read==m_size)
			m_read = 0;

		return 1;
	}

	inline int write(BufType data)
	{
		if (freeLen()<=0)
			return 0; 

		m_buf[m_write++] = data;
		m_produced++;
		
		if (m_write==m_size)
			m_write = 0;

		return 1;
	}

	uint32_t m_size;
	BufType *m_buf;
	uint32_t m_read;
	uint32_t m_write;
	uint32_t m_produced;
	uint32_t m_consumed;
};


// linear queue, to buffer a chunk and dispense it out
template <class BufType> class TransmitQ
{
public:
	TransmitQ(uint32_t size, SerialCallback callback)
	{
		m_size = size;
		m_buf = new BufType[m_size];
		m_read = 0;
		m_len = 0;
		m_callback = callback;
	}

   	~TransmitQ()
	{
		delete [] m_buf;
	}

	int read(BufType *data)
	{
		if (m_len==0)
		{
			m_len = (*m_callback)((uint8_t *)m_buf, m_size*sizeof(BufType))/sizeof(BufType);
			if (m_len==0)
				return 0;
		 	m_read = 0;
		}
		*data = m_buf[m_read++];
		m_len--;

		return 1;
	}

	uint32_t m_size;
	BufType *m_buf;
	uint32_t m_read;
	uint32_t m_len;
	SerialCallback m_callback;
};

// virtual interface to a serial device
class Iserial
{
public:
	virtual int open()
	{
		return 0;
	}
	virtual int close()
	{
		return 0;
	}
	virtual int receive(uint8_t *buf, uint32_t len)
	{
		return 0;
	}
	virtual int receiveLen()
	{
		return 0;
	}
	virtual int update()
	{
		return 0;
	}
};

#endif

