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

typedef uint32_t Qval;
struct QqueueFields;

#define QQ_LOC        SRAM4_LOC
#define QQ_SIZE       0x3000
#define QQ_MEM_SIZE  ((QQ_SIZE-sizeof(struct QqueueFields)+sizeof(Qval))/sizeof(Qval))

struct QqueueFields
{
    volatile uint16_t readIndex;
    volatile uint16_t writeIndex;

    volatile uint16_t produced;
    volatile uint16_t consumed;

    // (array size below doesn't matter-- we're just going to cast a pointer to this struct)
    volatile Qval data[1]; // data
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
    int enqueue(Qval val);
#endif

    uint32_t readAll(Qval *mem, uint32_t size);
    void flush();

private:
    QqueueFields *m_fields;
};

#else //  M0 is C and the "producer" of data (Qvals)

uint32_t qq_enqueue(Qval val);
uint16_t qq_free(void);

extern struct QqueueFields *g_qqueue;

#endif

#endif
