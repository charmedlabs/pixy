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

#include <string.h>
#include "../inc/qqueue.h" // need the relative path because Qt has the same file!
#ifdef PIXY
#include <pixyvals.h>
#endif

Qqueue::Qqueue()
{
#ifdef PIXY
    m_fields = (QqueueFields *)QQ_LOC;
#else
    m_fields = (QqueueFields *)(new uint8_t[QQ_SIZE]);
#endif
    memset((void *)m_fields, 0, sizeof(QqueueFields));
}

Qqueue::~Qqueue()
{
#ifdef PIXY
#else
    delete [] m_fields;
#endif
}

uint32_t Qqueue::dequeue(Qval *val)
{
    uint16_t len = m_fields->produced - m_fields->consumed;
    if (len)
    {
        *val = m_fields->data[m_fields->readIndex++];
        m_fields->consumed++;
        if (m_fields->readIndex==QQ_MEM_SIZE)
            m_fields->readIndex = 0;
        return 1;
    }
    return 0;
}

#ifndef PIXY
int Qqueue::enqueue(Qval *val)
{
    uint16_t len = m_fields->produced - m_fields->consumed;
    uint16_t freeLen = 	QQ_MEM_SIZE-len;
    if (freeLen>0)
    {
        m_fields->data[m_fields->writeIndex++] = *val;
        m_fields->produced++;
        if (m_fields->writeIndex==QQ_MEM_SIZE)
            m_fields->writeIndex = 0;
        return 1;
    }
    return 0;

}

#endif

uint32_t Qqueue::readAll(Qval *mem, uint32_t size)
{
    uint16_t len = m_fields->produced - m_fields->consumed;
    uint16_t i, j;

    for (i=0, j=m_fields->readIndex; i<len && i<size; i++)
    {
        mem[i] = m_fields->data[j++];
        if (j==QQ_MEM_SIZE)
            j = 0;
    }
    // flush the rest
    m_fields->consumed += len;
    m_fields->readIndex += len;
    if (m_fields->readIndex>=QQ_MEM_SIZE)
        m_fields->readIndex -= QQ_MEM_SIZE;

    return i;
}

void Qqueue::flush()
{
    uint16_t len = m_fields->produced - m_fields->consumed;

    m_fields->consumed += len;
    m_fields->readIndex += len;
    if (m_fields->readIndex>=QQ_MEM_SIZE)
        m_fields->readIndex -= QQ_MEM_SIZE;
}


