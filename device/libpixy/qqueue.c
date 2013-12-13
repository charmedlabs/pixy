#include "qqueue.h"
#include "pixyvals.h"

struct QqueueFields *g_qqueue = (struct QqueueFields *)QQ_LOC;

uint32_t qq_enqueue(Qval val)
{
    if (qq_free()>0)
    {
        g_qqueue->data[g_qqueue->writeIndex++] = val;
        g_qqueue->produced++;
		if (g_qqueue->writeIndex==QQ_SIZE)
			g_qqueue->writeIndex = 0;
        return 1;
    }
    return 0;
}

uint16_t qq_free(void)
{
    uint16_t len = g_qqueue->produced - g_qqueue->consumed;
	return QQ_SIZE-len;
} 
