#include <QFile>
#include <QTextStream>
#include <stdlib.h>
#include <math.h>
#include <QDebug>
#include "colorlut.h"


float sign(float val)
{
    if (val<0.0)
        return -1.0;
    else
        return 1.0;
}

float dot(Fpoint a, Fpoint b)
{
    return a.m_x*b.m_x + a.m_y*b.m_y;
}

ColorLUT::ColorLUT(const void *lutMem)
{
    m_lut = (uint8_t *)lutMem;
    m_iterateStep = CL_DEFAULT_ITERATE_STEP;
    m_hueTol = CL_DEFAULT_HUETOL;
    m_satTol = CL_DEFAULT_SATTOL;
    m_minSat = CL_DEFAULT_MINSAT;
    m_maxSatRatio = CL_DEFAULT_MAXSAT_RATIO;
    m_outlierRatio = CL_DEFAULT_OUTLIER_RATIO;

    clear();

}

ColorLUT::~ColorLUT()
{
}


int ColorLUT::generate(ColorModel *model, const uint8_t *bayerPixels, uint16_t xOffset, uint16_t yOffset, uint16_t width, uint16_t height, uint16_t pitch)
{
    Fpoint meanVal;
    float angle, pangle, pslope;
    float yi, istep, s, xsat, sat;

    m_hpixels = (HuePixel *)malloc(sizeof(HuePixel)*(width+1)*(height+1)/4);
    if (m_hpixels==NULL)
        return -1; // not enough memory

    map(bayerPixels, xOffset, yOffset, width, height, pitch);
    mean(&meanVal);
    angle = atan2(meanVal.m_y, meanVal.m_x);
    Fpoint uvec(cos(angle), sin(angle));

    Line hueLine(tan(angle), 0.0);

    pangle = angle + PI/2; // perpendicular angle
    pslope = tan(pangle); // perpendicular slope
    Line pLine(pslope, meanVal.m_y - pslope*meanVal.m_x); // perpendicular line through mean

    // upper hue line
    istep = fabs(m_iterateStep/uvec.m_x);
    yi = iterate(hueLine, istep);
    yi += fabs(m_hueTol*yi); // extend
    model->m_hue[0].m_yi = yi;
    model->m_hue[0].m_slope = hueLine.m_slope;

    // lower hue line
    yi = iterate(hueLine, -istep);
    yi -= fabs(m_hueTol*yi); // extend
    model->m_hue[1].m_yi = yi;
    model->m_hue[1].m_slope = hueLine.m_slope;

    // lower sat line
    s = sign(uvec.m_y);
    istep = s*fabs(m_iterateStep/cos(pangle));
    yi = iterate(pLine, -istep);
    yi -= s*fabs(m_satTol*(yi-pLine.m_yi)); // extend
    xsat = yi/(hueLine.m_slope-pslope);
    Fpoint minsatVec(xsat, xsat*hueLine.m_slope);
    sat = dot(uvec, minsatVec);
    if (sat < m_minSat)
    {
        minsatVec.m_x = uvec.m_x*m_minSat;
        minsatVec.m_y = uvec.m_y*m_minSat;
        yi = minsatVec.m_y - pslope*minsatVec.m_x;
    }
    model->m_sat[0].m_yi = yi;
    model->m_sat[0].m_slope = pslope;

    // upper sat line
    yi = iterate(pLine, istep);
    yi += s*fabs(m_maxSatRatio*m_satTol*(yi-pLine.m_yi)); // extend
    model->m_sat[1].m_yi = yi;
    model->m_sat[1].m_slope = pslope;

    if (model->m_sat[1].m_yi>model->m_sat[0].m_yi)
    {
        Line tmp = model->m_sat[0];
        model->m_sat[0] = model->m_sat[1];
        model->m_sat[1] = tmp;
    }
    matlabOut(model);

    free(m_hpixels);

    return 0;
}

void ColorLUT::map(const uint8_t *bayerPixels, uint16_t xOffset, uint16_t yOffset, uint16_t width, uint16_t height, uint16_t pitch)
{
    uint32_t x, y, r, g1, g2, b, count;
    int32_t u, v;

    xOffset |= 1;
    yOffset |= 1;

    bayerPixels += yOffset*pitch + xOffset;
    for (y=0, count=0; y<height; y+=2, bayerPixels+=pitch*2)
    {
        for (x=0; x<width; x+=2, count++)
        {
            r = bayerPixels[x];
            g1 = bayerPixels[x - 1];
            g2 = bayerPixels[-pitch + x];
            b = bayerPixels[-pitch + x - 1];
            u = r-g1;
            v = b-g2;
            u >>= 1;
            v >>= 1;
            m_hpixels[count].m_u = u;
            m_hpixels[count].m_v = v;
        }
    }
    m_hpixelLen = count;

}

void ColorLUT::tweakMean(float *mean)
{
    if (abs(*mean)<CL_MIN_MEAN)
    {
        if (*mean>0.0)
            *mean = CL_MIN_MEAN;
        else
            *mean = -CL_MIN_MEAN;
    }
}

void ColorLUT::mean(Fpoint *mean)
{
    uint32_t i;
    float usum, vsum;

    for (i=0, usum=0.0, vsum=0.0; i<m_hpixelLen; i++)
    {
        usum += m_hpixels[i].m_u;
        vsum += m_hpixels[i].m_v;
    }
    usum /= m_hpixelLen;
    vsum /= m_hpixelLen;

    tweakMean(&usum);
    tweakMean(&vsum);

    mean->m_x = usum;
    mean->m_y = vsum;
}

uint32_t ColorLUT::boundTest(const Line *line, float dir)
{
    uint32_t i, count;
    float v;
    bool gtz = dir>0.0;

    for (i=0, count=0; i<m_hpixelLen; i++)
    {
        v = m_hpixels[i].m_u*line->m_slope + line->m_yi;
        if (gtz)
        {
            if (m_hpixels[i].m_v<v)
                count++;
        }
        else if (m_hpixels[i].m_v>v)
            count++;
    }

    return count;
}

float ColorLUT::iterate(Line line, float step)
{
    float ratio;

    while(1)
    {
        ratio = (float)boundTest(&line, sign(step))/m_hpixelLen;
        qDebug() << ratio;
        if ( ratio >= m_outlierRatio)
            break;
        line.m_yi += step;
    }

    return line.m_yi;
}


int ColorLUT::setBounds(float minSat, float hueTol, float satTol)
{
    m_minSat = minSat;
    m_hueTol = hueTol;
    m_satTol = satTol;

    return 0;
}

void ColorLUT::add(const ColorModel *model, uint8_t modelIndex)
{
    uint32_t i;
    HuePixel p;

    for (i=0; i<0x10000; i++)
    {
        p.m_v = (int8_t)(i&0xff);
        p.m_u = (int8_t)(i>>8);
        if (((m_lut[i]&0x07)==0 || (m_lut[i]&0x07)>=modelIndex) &&
                    checkBounds(model, &p))
             m_lut[i] = modelIndex;
    }
    matlabOut();
}

bool ColorLUT::checkBounds(const ColorModel *model, const HuePixel *pixel)
{
    float v;

    v = model->m_hue[0].m_slope*pixel->m_u + model->m_hue[0].m_yi;
    if (v<(float)pixel->m_v)
        return false;

    v = model->m_hue[1].m_slope*pixel->m_u + model->m_hue[1].m_yi;
    if (v>(float)pixel->m_v)
        return false;

    v = model->m_sat[0].m_slope*pixel->m_u + model->m_sat[0].m_yi;
    if (v<(float)pixel->m_v)
        return false;

    v = model->m_sat[1].m_slope*pixel->m_u + model->m_sat[1].m_yi;
    if (v>(float)pixel->m_v)
        return false;

    return true;
}

void ColorLUT::clear(uint8_t modelIndex)
{
    uint32_t i;

    for (i=0; i<0x10000; i++)
    {
        if (modelIndex==0 || (m_lut[i]&0x07)==modelIndex)
            m_lut[i] = 0;
    }
}

void ColorLUT::matlabOut(const ColorModel *model)
{
    unsigned int i;
    QString str, name = "lutinfo";
    QFile file(name + ".m");
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QTextStream out(&file);

    out << "function [HuePixels, Lines]=" << name << "()\n\n";
    out << "HuePixels=[\n";
    for (i=0; i<m_hpixelLen; i++)
        out << str.sprintf("%d %d\n", m_hpixels[i].m_u, m_hpixels[i].m_v);

    out << "];\n\n";
    out << "Lines=[\n";
    out << str.sprintf("%f %f\n",  model->m_hue[0].m_slope,  model->m_hue[0].m_yi);
    out << str.sprintf("%f %f\n",  model->m_hue[1].m_slope,  model->m_hue[1].m_yi);
    out << str.sprintf("%f %f\n",  model->m_sat[0].m_slope,  model->m_sat[0].m_yi);
    out << str.sprintf("%f %f\n",  model->m_sat[1].m_slope,  model->m_sat[1].m_yi);
    out << "];\n";

    file.close();
}

void ColorLUT::matlabOut()
{
    unsigned int i;
    QString str, name = "lut";
    QFile file(name + ".m");
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QTextStream out(&file);

    out << "function [LUT]=" << name << "()\n\n";
    out << "LUT=[\n";
    for (i=0; i<0x10000; i++)
        out << str.sprintf("%d\n", m_lut[i]);

    out << "];\n";

    file.close();
}
