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
#ifndef COLORLUT_H
#define COLORLUT_H	  

#include <inttypes.h>
#include "pixytypes.h"

#undef PI
#define PI 3.1415926f

#define CL_NUM_SIGNATURES               7
#define CL_LUT_SIZE                     0x1000
#define LUT_ENTRY_SCALE                 15

struct ColorSignature
{
    int32_t m_uMin;
    int32_t m_uMax;
    int32_t m_uMean;
    int32_t m_vMin;
    int32_t m_vMax;
    int32_t m_vMean;
};

struct RuntimeSignature
{
    int32_t m_uMin;
    int32_t m_uMax;
    int32_t m_vMin;
    int32_t m_vMax;
};

class ColorLUT
{
public:
    ColorLUT(uint8_t *lut);
    ~ColorLUT();

    ColorSignature m_signatures[CL_NUM_SIGNATURES];
    RuntimeSignature m_runtimeSigs[CL_NUM_SIGNATURES];
	uint8_t *m_lut;
};

#endif // COLORLUT_H
