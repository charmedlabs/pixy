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

#ifdef PIXY
#define QQ_SIZE       MEM_QQ_SIZE
#else
#define QQ_SIZE       0x30000
#endif
#define QQ_MEM_SIZE  ((QQ_SIZE - sizeof(struct QqueueFields) + sizeof(Qval)) / sizeof(Qval))

#define QVAL_LINE_BEGIN    0xfffd
#define QVAL_FRAME_ERROR   0xfffe
#define QVAL_FRAME_END     0xffff

#ifdef __cplusplus
struct Qval
#else
typedef struct
#endif
{
#ifdef __cplusplus
    Qval()
    {
        m_col_start = m_col_end = 0;
    }

    Qval(uint16_t start, uint16_t end)
    {
        m_col_start = start;
        m_col_end = end;
    }
#endif

    uint16_t m_col_start;
    uint16_t m_col_end;  // Not inclusive

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

void qq_init();
uint32_t qq_enqueue(const Qval *val);
uint16_t qq_free(void);

extern struct QqueueFields *g_qqueue;

#endif

#endif
