#ifndef _QQUEUE_H
#define _QQUEUE_H
#include <inttypes.h>

typedef uint32_t Qval;
struct QqueueFields;

#define QQ_LOC        SRAM4_LOC
#define QQ_SIZE       0x3000
#define QQ_MEM_SIZE  ((QQ_SIZE-sizeof(struct QqueueFields)+sizeof(Qval))/sizeof(Qval))

struct QqueueFields
{
    uint16_t readIndex;
    uint16_t writeIndex;

    uint16_t produced;
    uint16_t consumed;

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
