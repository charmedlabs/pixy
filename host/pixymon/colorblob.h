#ifndef CBLOB_H
#define CBLOB_H
#include <inttypes.h>
#include "pixytypes.h"

#define NUM_SIGNATURES               7
#define LUT_COMPONENT_SCALE          6
#define LUT_SIZE                     (1<<(LUT_COMPONENT_SCALE*2))
#define LUT_ENTRY_SCALE              15
#define DEFAULT_TOL                  0.9f


struct ColorSignature
{
    int32_t m_uMin;
    int32_t m_uMax;
    int32_t m_vMin;
    int32_t m_vMax;
};

struct UVPixel
{
    UVPixel()
    {
        m_u = m_v = 0;
    }

    UVPixel(int32_t u, int32_t v)
    {
        m_u = u;
        m_v = v;
    }

    int32_t m_u;
    int32_t m_v;
};

class ColorBlob
{
public:
    ColorBlob(uint8_t *lut);
    ~ColorBlob();

    int generateSignature(const Frame8 *frame, const RectA *region, ColorSignature *signature);
    int generateSignature(const Frame8 *frame, const Point16 *point, ColorSignature *signature);

    void generateLUT(const ColorSignature *signature, uint8_t signum);
    void clearLUT(uint8_t signum=0);

private:
    float calcRatio(const int32_t *uvPixels, uint32_t numuv, int32_t line, bool lt);
    int32_t iterate(const int32_t *uvPixels, uint32_t numuv, float ratio, bool pos);
    ColorSignature m_signatures[NUM_SIGNATURES];
    uint8_t *m_lut;

    int32_t m_delta;
    float m_tol;
    float m_minSat;
    float m_acqRange;
    float m_trackRange;
};

#endif // CBLOB_H
