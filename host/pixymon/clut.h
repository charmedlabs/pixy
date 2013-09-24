#ifndef CLUT_H
#define CLUT_H
#include "stdint.h"

#define PI 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068

class CLUT
{
public:
    static void generateFromImgSample(uint32_t *data, int d_len, uint8_t* tempLut);
};

#endif // CLUT_H
