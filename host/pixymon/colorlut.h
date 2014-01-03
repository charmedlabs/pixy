#ifndef COLORLUT_H
#define COLORLUT_H

#include <inttypes.h>

#undef PI
#define PI 3.1415926

#define CL_DEFAULT_ITERATE_STEP         0.1
#define CL_DEFAULT_HUETOL               1.0
#define CL_DEFAULT_SATTOL               1.0
#define CL_DEFAULT_MINSAT               15.0
#define CL_DEFAULT_MAXSAT_RATIO         2.0
#define CL_DEFAULT_OUTLIER_RATIO        0.90
#define CL_MIN_MEAN                     0.001

struct HuePixel
{
    HuePixel()
    {
        m_u = m_v = 0;
    }

    HuePixel(int8_t u, int8_t v)
    {
        m_u = u;
        m_v = v;
    }

    int8_t m_u;
    int8_t m_v;
};

struct Fpoint
{
    Fpoint(float x, float y)
    {
        m_x = x;
        m_y = y;
    }

    float m_x;
    float m_y;
};

struct Line
{
    Line()
    {
        m_slope = m_yi = 0.0;
    }
    Line(float slope, float yi)
    {
        m_slope = slope;
        m_yi = yi;
    }

    float m_slope;
    float m_yi;
};

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

    int generate(ColorModel *model, const uint8_t *bayerPixels, uint16_t xOffset, uint16_t yOffset, uint16_t width, uint16_t height, uint16_t pitch);
    void add(const ColorModel *model, uint8_t modelIndex);
    void clear(uint8_t modelIndex=0); // 0 = all models

private:
    void map(const uint8_t *bayerPixels, uint16_t xOffset, uint16_t yOffset, uint16_t width, uint16_t height, uint16_t pitch);
    void mean(HuePixel *meanVal);
    float iterate(Line line, float step);
    void tweakMean(float *mean);
    uint32_t boundTest(const Line *line, float dir);
    bool checkBounds(const ColorModel *model, const HuePixel *pixel);

    void matlabOut(const ColorModel *model);
    void matlabOut();


    uint8_t *m_lut;
    HuePixel *m_hpixels;
    uint32_t m_hpixelLen;

    float m_iterateStep;
    float m_hueTol;
    float m_satTol;
    float m_minSat;
    float m_maxSatRatio;
    float m_outlierRatio;
};

#endif // COLORLUT_H
