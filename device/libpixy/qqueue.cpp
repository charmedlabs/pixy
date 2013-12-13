#include <string.h>
#include "qqueue.h"
#include <pixyvals.h>

Qqueue::Qqueue()
{
    m_fields = (QqueueFields *)QQ_LOC;
	memset((void *)m_fields, 0, sizeof(QqueueFields));
}

Qqueue::~Qqueue()
{
}

uint32_t Qqueue::dequeue(uint32_t *val)
{
    uint16_t len = m_fields->produced - m_fields->consumed;
    if (len)
    {
        *val = m_fields->data[m_fields->readIndex++];
        m_fields->consumed++;
		if (m_fields->readIndex==QQ_SIZE)
			m_fields->readIndex = 0;
        return 1;
    }
    return 0;
}

uint32_t Qqueue::readAll(Qval *mem, uint32_t size)
{
    uint16_t len = m_fields->produced - m_fields->consumed;
	uint16_t i, j;

	if (len>size)
		return 0;

	for (i=0, j=m_fields->readIndex; i<len; i++)
	{
		mem[i] = m_fields->data[j++];
		if (j==QQ_SIZE)
			j = 0;
	}
	m_fields->consumed += len;
	m_fields->readIndex += len;
	if (m_fields->readIndex>=QQ_SIZE)
		m_fields->readIndex -= QQ_SIZE;

	return len;					
}

