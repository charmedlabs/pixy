#include <math.h>
#include "colorblob.h"

ColorBlob::ColorBlob(uint8_t *lut)
{
    m_tol = DEFAULT_TOL;
    m_lut = lut;
    m_minSat = 2.0;
    m_acqRange = 2.0;
    m_trackRange = 1.0;
}

ColorBlob::~ColorBlob()
{
}

// lt==true means number of pixels less than line
float ColorBlob::calcRatio(const int32_t *uvPixels, uint32_t numuv, int32_t line, bool lt)
{
    uint32_t i;
    uint32_t count;

    for (i=0, count=0; i<numuv; i++)
    {
        if ((lt && uvPixels[i]<line) || (!lt && uvPixels[i]>line))
            count++;
    }

    return (float)count/numuv;
}

int32_t ColorBlob::iterate(const int32_t *uvPixels, uint32_t numuv, float ratio, bool pos)
{
    int32_t scale, line;
    float ri;

    for (scale=1<<30, line=0; scale!=0; scale>>=1)
    {
        ri = calcRatio(uvPixels, numuv, line, pos);
        if (pos)
        {
            if (ri>ratio)
                line -=scale;
            else
                line += scale;
        }
        else
        {
            if (ri>ratio)
                line +=scale;
            else
                line -= scale;
        }
    }

    return line;
}

int ColorBlob::generateSignature(const Frame8 *frame, const RectA *region, ColorSignature *signature)
{
    int32_t x, y, r, g1, g2, b, count, u, v;
    uint8_t *pixels;
    int32_t numuv = (region->m_width/2 + 1)*(region->m_height/2 + 1);
    int32_t *uPixels = new int32_t[numuv];
    int32_t *vPixels = new int32_t[numuv];

    pixels = frame->m_pixels + (region->m_yOffset | 1)*frame->m_width + (region->m_xOffset | 1);
    for (y=0, count=0; y<region->m_height && count<numuv; y+=2, pixels+=frame->m_width*2)
    {
        for (x=0; x<region->m_width && count<numuv; x+=2, count++)
        {
            r = pixels[x];
            g1 = pixels[x - 1];
            g2 = pixels[-frame->m_width + x];
            b = pixels[-frame->m_width + x - 1];
            u = ((r-g1)<<LUT_ENTRY_SCALE)/(r+g1+b);
            v = ((b-g2)<<LUT_ENTRY_SCALE)/(r+g2+b);
            uPixels[count] = u;
            vPixels[count] = v;
        }
    }

    signature->m_uMin = iterate(uPixels, count, m_tol, false);
    signature->m_uMax = iterate(uPixels, count, m_tol, true);
    signature->m_vMin = iterate(vPixels, count, m_tol, false);
    signature->m_vMax = iterate(vPixels, count, m_tol, true);

    delete [] uPixels;
    delete [] vPixels;

    return 0;
}

int ColorBlob::generateSignature(const Frame8 *frame, const Point16 *point, ColorSignature *signature)
{
    return 0;
}

void ColorBlob::generateLUT(const ColorSignature *signature, uint8_t signum)
{
    int32_t u, v, i, j, bin;
    float bratios[4], umin, umax, vmin, vmax, ratio, sat, minRatio, maxRatio;

    clearLUT(signum);

    umin = signature->m_uMin*m_acqRange*m_trackRange;
    umax = signature->m_uMax*m_acqRange*m_trackRange;
    vmin = signature->m_vMin*m_acqRange*m_trackRange;
    vmax = signature->m_vMax*m_acqRange*m_trackRange;
    bratios[0] = vmin/umin;
    bratios[1] = vmin/umax;
    bratios[2] = vmax/umin;
    bratios[3] = vmax/umax;

    for (i=0, maxRatio=-10000.0f, minRatio=10000.0f; i<4; i++)
    {
        if (bratios[i]>maxRatio)
            maxRatio = bratios[i];
        if (bratios[i]<minRatio)
            minRatio = bratios[i];
    }

    for (i=0, bin=0; i<(1<<LUT_COMPONENT_SCALE); i++)
    {
        for (j=0; j<(1<<LUT_COMPONENT_SCALE); j++, bin++)
        {
            u = (i<<(32-LUT_COMPONENT_SCALE))>>(32-LUT_COMPONENT_SCALE); // sign extend
            v = (j<<(32-LUT_COMPONENT_SCALE))>>(32-LUT_COMPONENT_SCALE); // sign extend

            sat = sqrt((float)(u*u) + (float)(v*v));
            ratio = (float)v/u;
            if (minRatio<ratio && ratio<maxRatio && sat>m_minSat)
            {
                if ((umin>0.0f)==(umax>0.0f))
                {
                    if ((u>0)==(umin>0.0f))
                    {
                        if (m_lut[bin]==0 || m_lut[bin]>signum)
                            m_lut[bin] = signum;
                    }
                }
                else
                {
                    if ((v>0)==(vmin>0.0f))
                    {
                        if (m_lut[bin]==0 || m_lut[bin]>signum)
                            m_lut[bin] = signum;
                    }
                }
            }
        }
    }
}

void ColorBlob::clearLUT(uint8_t signum)
{
    int i;

    for (i=0; i<LUT_SIZE; i++)
    {
        if (signum==0)
            m_lut[i] = 0;
        else if (m_lut[i]==signum)
            m_lut[i] = 0;
    }
}



