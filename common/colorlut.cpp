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

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "colorlut.h"


void *maxMalloc(uint32_t initSize, uint32_t *allocSize)
{
    void *mem;
    int32_t size = (int32_t)initSize;

    while(size>=0)
    {
        mem = malloc(size);
        if (mem)
        {
            *allocSize = (uint32_t)size;
            return mem;
        }
        else
            size -= 0x100;
    }
    *allocSize = 0;
    return NULL;
}

ColorLUT::ColorLUT(uint8_t *lut)
{
    m_lut = lut;
	memset((void *)m_signatures, 0, sizeof(ColorSignature)*CL_NUM_SIGNATURES);
	memset((void *)m_runtimeSigs, 0, sizeof(RuntimeSignature)*CL_NUM_SIGNATURES);
}

ColorLUT::~ColorLUT()
{
}


