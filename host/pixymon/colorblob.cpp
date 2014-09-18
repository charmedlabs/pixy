#include <math.h>
#include <QDebug>
#include "colorblob.h"

ColorBlob::ColorBlob(uint8_t *lut)
{
    m_tol = DEFAULT_TOL;
    m_lut = lut;
    m_minSat = 2.0;
    m_acqRange = 2.0;
    m_trackRange = 1.0;
    clearLUT();
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

// This is a binary search --- it's guaranteed get to within +/- 1, which is good enough
int32_t ColorBlob::iterate(const int32_t *uvPixels, uint32_t numuv, float ratio, bool pos)
{
    int32_t scale, line;
    float ri;

    //qDebug()<< "begin";
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
        //qDebug("%f %d", ri, line);
    }

    //qDebug()<< "end";
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

    // if signs of u's and v's are *both* different, our envelope is greater than 90 degrees and it's an indication
    // that we don't have much of a lock
    if ((signature->m_uMin>0)!=(signature->m_uMax>0) && (signature->m_vMin>0)!=(signature->m_vMax>0))
    {
        signature->m_uMin = 0;
        signature->m_uMax = 0;
        signature->m_vMin = 0;
        signature->m_vMax = 0;
        return -1;
    }
    return 0;
}

int ColorBlob::generateSignature(const Frame8 *frame, const Point16 *point, ColorSignature *signature)
{
    return 0;
}

int ColorBlob::generateLUT(const ColorSignature *signature, uint8_t signum)
{
    int32_t u, v, i, j, bin, signChange=0;
    bool u0, v0, lutVal;
    float bratios[4], c, umin, umax, vmin, vmax, ratio, sat, minRatio, maxRatio;

    clearLUT(signum);

    // scale up
    c = ((float)signature->m_uMax + signature->m_uMin)/2.0f;
    umin = c + (signature->m_uMin - c)*m_acqRange*m_trackRange;
    umax = c + (signature->m_uMax - c)*m_acqRange*m_trackRange;
    c = ((float)signature->m_vMax + signature->m_vMin)/2.0f;
    vmin = c + (signature->m_vMin - c)*m_acqRange*m_trackRange;
    vmax = c + (signature->m_vMax - c)*m_acqRange*m_trackRange;

    // count the sign changes
    if ((umin>0.0f)!=(signature->m_uMin>0))
        signChange++;
    if ((umax>0.0f)!=(signature->m_uMax>0))
        signChange++;
    if ((vmin>0.0f)!=(signature->m_vMin>0))
        signChange++;
    if ((vmax>0.0f)!=(signature->m_vMax>0))
        signChange++;

    // if we've changed signs more than once, we've overflowed
    if (signChange>1)
        return -1; // overflow

    // calculate ratios
    bratios[0] = vmin/umin;
    bratios[1] = vmin/umax;
    bratios[2] = vmax/umin;
    bratios[3] = vmax/umax;

    // find max/min ratio values
    if ((umin>0.0f)==(umax>0.0f))
    {
        // find normal max and min
        for (i=0, maxRatio=-10000.0f, minRatio=10000.0f; i<4; i++)
        {
            if (bratios[i]>maxRatio)
                maxRatio = bratios[i];
            if (bratios[i]<minRatio)
                minRatio = bratios[i];
        }
    }
    else // special case where lines straddle the y axis
    {
        // find smallest positive and largest negative
        for (i=0, maxRatio=10000.0f, minRatio=-10000.0f; i<4; i++)
        {
            if (bratios[i]>0 && bratios[i]<maxRatio) // positive and less than
                maxRatio = bratios[i];
            if (bratios[i]<0 && bratios[i]>minRatio) // negative and greater than
                minRatio = bratios[i];
        }
    }

    for (i=0, bin=0; i<(1<<LUT_COMPONENT_SCALE); i++)
    {
        for (j=0; j<(1<<LUT_COMPONENT_SCALE); j++, bin++)
        {
            u = (i<<(32-LUT_COMPONENT_SCALE))>>(32-LUT_COMPONENT_SCALE); // sign extend and shift right/divide
            v = (j<<(32-LUT_COMPONENT_SCALE))>>(32-LUT_COMPONENT_SCALE); // sign extend and shift right/divide

            u0 = u>0;
            v0 = v>0;
            ratio = (float)v/u;
            sat = sqrt((float)(u*u) + (float)(v*v));

            if (sat<m_minSat)
                continue;

            // knock off upper bits
            u &= (1<<LUT_COMPONENT_SCALE)-1;
            v &= (1<<LUT_COMPONENT_SCALE)-1;

            lutVal = false;
            if ((umin>0.0f)==(umax>0.0f)) // left or right half of hue plane
            {
                if (u0==(umin>0.0f) && minRatio<ratio && ratio<maxRatio) // make sure sign of u is the same is umin
                    lutVal = true;
            }
            else if (v0==(vmin>0.0f)) // make sure signe of v is the same as vmin  top or bottom
            {
                if (maxRatio<ratio || ratio<minRatio) // in this region, the ratio signs change based on which half of the hue plane you're on
                    lutVal = true;
            }

            if (lutVal && (m_lut[bin]==0 || m_lut[bin]>signum))
                m_lut[bin] = signum;

        }
    }
    return 0;
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



