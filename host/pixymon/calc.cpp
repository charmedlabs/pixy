#include "calc.h"

#define MAX(a, b)  (a>b ? a : b)
#define MIN(a, b)  (a<b ? a : b)

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

