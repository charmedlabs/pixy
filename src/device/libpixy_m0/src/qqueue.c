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

#include "qqueue.h"
#include "pixyvals.h"

struct QqueueFields *g_qqueue = (struct QqueueFields *)QQ_LOC;

uint32_t qq_enqueue(const Qval *val)
{
    if (qq_free()>0)
    {
        g_qqueue->data[g_qqueue->writeIndex++] = *val;
        g_qqueue->produced++;
		if (g_qqueue->writeIndex==QQ_MEM_SIZE)
			g_qqueue->writeIndex = 0;
        return 1;
    }
    return 0;
}

uint16_t qq_free(void)
{
    uint16_t len = g_qqueue->produced - g_qqueue->consumed;
	return QQ_MEM_SIZE-len;
} 
