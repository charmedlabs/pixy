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

#define CL_LUT_SIZE                     0x10000
#define CL_DEFAULT_ITERATE_STEP         0.1f
#define CL_DEFAULT_HUETOL               1.0f
#define CL_DEFAULT_SATTOL               1.0f
#define CL_DEFAULT_MINSAT               15.0f
#define CL_DEFAULT_MAXSAT_RATIO         2.0f
#define CL_DEFAULT_OUTLIER_RATIO        0.90f
#define CL_MIN_MEAN                     0.001f
#define CL_HPIXEL_MAX_SIZE              10000

struct ColorModel
{
    Line m_hue[2];
    Line m_sat[2];
};


class ColorLUT
{
public:
    ColorLUT(const void *lutMem);
    ~ColorLUT();

    int setBounds(float minSat, float hueTol, float satTol);
    int setOther(float maxSatRatio, float outlierRatio);

    int generate(ColorModel *model, const Frame8 &frame, const RectA &region);
    int growRegion(RectA *result, const Frame8 &frame, const Point16 &seed);
    void add(const ColorModel *model, uint8_t modelIndex);
    void clear(uint8_t modelIndex=0); // 0 = all models

private:
    void map(const Frame8 &frame, const RectA &region);
    void mean(Fpoint *meanVal);
    float iterate(Line line, float step);
    void tweakMean(float *mean);
    uint32_t boundTest(const Line *line, float dir);
    bool checkBounds(const ColorModel *model, const HuePixel *pixel);

#ifndef PIXY
    void matlabOut(const ColorModel *model);
    void matlabOut();
#endif

    uint8_t *m_lut;
    HuePixel *m_hpixels;
    uint32_t m_hpixelLen;  // number of pixels
    uint32_t m_hpixelSize; // size of m_hpixels memory in HuePixels

    float m_iterateStep;
    float m_hueTol;
    float m_satTol;
    float m_minSat;
    float m_maxSatRatio;
    float m_outlierRatio;
};

#endif // COLORLUT_H
