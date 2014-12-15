#ifndef CBLOB_H
#define CBLOB_H
#include <inttypes.h>
#include "pixytypes.h"
#include "simplevector.h"

#define NUM_SIGNATURES               7
#define LUT_COMPONENT_SCALE          6
#define LUT_SIZE                     (1<<(LUT_COMPONENT_SCALE*2))
#define LUT_ENTRY_SCALE              15
#define DEFAULT_TOL                  0.9f
#define DEFAULT_RANGE                2.0f
#define DEFAULT_MINY                 0.1f
#define MIN_RATIO                    0.25f
#define MAX_DIST                     2000
#define GROW_INC                     4
#define MIN_Y_F                      0.05 // for when generating signatures, etc
#define MIN_Y                        (int32_t)(3*((1<<8)-1)*MIN_Y_F)


typedef SimpleVector<Point16> Points;

#if 1
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
#endif

struct ColorSignature;
struct RuntimeSignature;

class IterPixel2;

class ColorBlob
{
public:
    ColorBlob(uint8_t *lut);
    ~ColorBlob();

    int generateSignature(const Frame8 &frame, const RectA &region, ColorSignature *signature);
    int generateSignature(const Frame8 &frame, const Point16 &point, Points *points, ColorSignature *signature);
    int generateSignature2(const Frame8 &frame, const RectA &region, ColorSignature *signature);
    int generateSignature2(const Frame8 &frame, const Point16 &point, Points *points, ColorSignature *signature);

    int generateLUT(const RuntimeSignature signatures[]);
    void clearLUT(uint8_t signum=0);

    void setParameters(float range, float miny, uint32_t maxDist, float minRatio);

private:
    bool growRegion(RectA *region, const Frame8 &frame, uint8_t dir);
    float testRegion(const RectA &region, const Frame8 &frame, Point32 *mean, Points *points);
    void getMean(const RectA &region ,const Frame8 &frame, Point32 *mean);
    void growRegion(const Frame8 &frame, const Point16 &seed, Points *points);

    float calcRatio(const int32_t *uvPixels, uint32_t numuv, int32_t line, bool lt);
    int32_t iterate(const int32_t *uvPixels, uint32_t numuv, float ratio, bool pos);

    void calcRatios(IterPixel2 *ip, ColorSignature *sig, float ratios[]);
    void iterate(IterPixel2 *ip, ColorSignature *sig);

    uint8_t *m_lut;

    int32_t m_delta;
    float m_tol;
    float m_minSat;
    float m_miny;
    float m_acqRange;
    float m_trackRange;
    uint32_t m_maxDist;
    float m_minRatio;
    float m_ratio;
};

#endif // CBLOB_H
