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

#include "calc.h"

void hsvc(uint8_t r, uint8_t g, uint8_t b, uint8_t *h, uint8_t *s, uint8_t *v, uint8_t *c)
{
    uint8_t min, max, delta;
    int hue;
    min = MIN(r, g);
    min = MIN(min, b);
    max = MAX(r, g);
    max = MAX(max, b);

    *v = max;
    delta = max - min;
    if (max>50)
    {
        //if (delta>50)
            *s = ((int)delta<<8)/max;
        //else
        //    *s = 0;
    }
    else
        *s = 0;
    if (max==0 || delta==0)
    {
        *s = 0;
        *h = 0;
        *c = 0;
        return;
    }
    if (r==max)
        hue = (((int)g - (int)b)<<8)/delta;         // between yellow & magenta
    else if (g==max)
        hue = (2<<8) + (((int)b - (int)r)<<8)/delta;     // between cyan & yellow
    else
        hue = (4<<8) + (((int)r - (int)g)<<8)/delta;     // between magenta & cyan
    if(hue < 0)
        hue += 6<<8;
    hue /= 6;
    *h = hue;
    *c = delta;
}

uint32_t lighten(uint32_t color, uint8_t factor)
{
    uint32_t r, g, b;

    rgbUnpack(color, &r, &g, &b);

    r += factor;
    g += factor;
    b += factor;

    return rgbPack(r, g, b);
}

uint32_t rgbPack(uint32_t r, uint32_t g, uint32_t b)
{
    if (r>0xff)
        r = 0xff;
    if (g>0xff)
        g = 0xff;
    if (b>0xff)
        b = 0xff;
    return (r<<16) | (g<<8) | b;
}

void rgbUnpack(uint32_t color, uint32_t *r, uint32_t *g, uint32_t *b)
{
    *b = color&0xff;
    color >>= 8;
    *g = color&0xff;
    color >>= 8;
    *r = color&0xff;
}

uint32_t saturate(uint32_t color)
{
    float m;
    uint32_t max, r, g, b;

    rgbUnpack(color, &r, &g, &b);

    max = MAX(r, g);
    max = MAX(max, b);

    // saturate while maintaining ratios
    m = 255.0f/max;
    r = (uint8_t)(m*r);
    g = (uint8_t)(m*g);
    b = (uint8_t)(m*b);

    return rgbPack(r, g, b);
}
