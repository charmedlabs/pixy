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
#ifndef _QQUEUE_H
#define _QQUEUE_H
#include <stdint.h>

#define QQ_LOC        SRAM4_LOC
#ifdef PIXY
#define QQ_SIZE       0x3c00
#else
#define QQ_SIZE       0x30000
#endif
#define QQ_MEM_SIZE  ((QQ_SIZE-sizeof(struct QqueueFields)+sizeof(Qval))/sizeof(Qval))

#ifdef __cplusplus  
struct Qval
#else
typedef struct
#endif
{
#ifdef __cplusplus  
    Qval()
    {
        m_u = m_v = m_y = m_col = 0;
    }

    Qval(int16_t u, int16_t v, uint16_t y, uint16_t col)
    {
        m_u = u;
        m_v = v;
        m_y = y;
        m_col = col;
    }
#endif

    uint16_t m_col;
    int16_t m_v;
    int16_t m_u;
    uint16_t m_y;

#ifdef __cplusplus  
}; 
#else
} Qval;
#endif


struct QqueueFields
{
    volatile uint16_t readIndex;
    volatile uint16_t writeIndex;

    volatile uint16_t produced;
    volatile uint16_t consumed;

    // (array size below doesn't matter-- we're just going to cast a pointer to this struct)
    Qval data[1]; // data
};

#ifdef __cplusplus  // M4 is C++ and the "consumer" of data

class Qqueue
{
public:
    Qqueue();
    ~Qqueue();

    uint32_t dequeue(Qval *val);
	uint32_t queued()
	{
		return m_fields->produced - m_fields->consumed;
	}
#ifndef PIXY
    int enqueue(Qval *val);
#endif

    uint32_t readAll(Qval *mem, uint32_t size);
    void flush();

private:
    QqueueFields *m_fields;
};

#else //  M0 is C and the "producer" of data (Qvals)

uint32_t qq_enqueue(const Qval *val);
uint16_t qq_free(void);

extern struct QqueueFields *g_qqueue;

#endif

#endif
