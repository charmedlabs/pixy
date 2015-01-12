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

#ifndef CALC_H
#define CALC_H
#include <inttypes.h>

#ifdef MAX
#undef MAX
#endif
#ifdef MIN
#undef MIN
#endif
#define MAX(a, b)  (a>b ? a : b)
#define MIN(a, b)  (a<b ? a : b)

void hsvc(uint8_t r, uint8_t g, uint8_t b, uint8_t *h, uint8_t *s, uint8_t *v, uint8_t *c);
uint32_t lighten(uint32_t color, uint8_t factor);
uint32_t saturate(uint32_t color);
uint32_t rgbPack(uint32_t r, uint32_t g, uint32_t b);
void rgbUnpack(uint32_t color, uint32_t *r, uint32_t *g, uint32_t *b);

#endif // CALC_H
