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

float distance(Fpoint a, Fpoint b)
{
    float diffx, diffy;

    diffx = a.m_x-b.m_x;
    diffy = a.m_y-b.m_y;

    return sqrt(diffx*diffx + diffy*diffy);
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


int ColorLUT::generate(ColorModel *model, const Frame8 &frame, const RectA &region)
{
    Fpoint meanVal;
    float angle, pangle, pslope;
    float yi, istep, s, xsat, sat;

    m_hpixels = (HuePixel *)malloc(sizeof(HuePixel)*CL_HPIXEL_MAX_SIZE);
    if (m_hpixels==NULL)
        return -1; // not enough memory

    m_hpixelSize = CL_HPIXEL_MAX_SIZE;

    map(frame, region);
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

    // inner sat line
    s = sign(uvec.m_y);
    istep = s*fabs(m_iterateStep/cos(pangle));
    yi = iterate(pLine, -istep);
    yi -= s*fabs(m_satTol*(yi-pLine.m_yi)); // extend
    xsat = yi/(hueLine.m_slope-pslope); // x value where inner sat line crosses hue line
    Fpoint minsatVec(xsat, xsat*hueLine.m_slope); // vector going to inner sat line
    sat = dot(uvec, minsatVec); // length of line
    if (sat < m_minSat) // if it's too short, we need to extend
    {
        minsatVec.m_x = uvec.m_x*m_minSat;
        minsatVec.m_y = uvec.m_y*m_minSat;
        yi = minsatVec.m_y - pslope*minsatVec.m_x;
    }
    model->m_sat[0].m_yi = yi;
    model->m_sat[0].m_slope = pslope;

    // outer sat line
    yi = iterate(pLine, istep);
    yi += s*fabs(m_maxSatRatio*m_satTol*(yi-pLine.m_yi)); // extend
    model->m_sat[1].m_yi = yi;
    model->m_sat[1].m_slope = pslope;

    // swap if outer sat line is greater than inner sat line
    // Arbitrary convention, but we need it to be consistent to test membership (checkBounds)
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

void ColorLUT::map(const Frame8 &frame, const RectA &region)
{
    uint32_t x, y, r, g1, g2, b, count;
    int32_t u, v;
    uint8_t *pixels;

    pixels = frame.m_pixels + (region.m_yOffset | 1)*frame.m_width + (region.m_xOffset | 1);
    for (y=0, count=0; y<region.m_height && count<m_hpixelSize; y+=2, pixels+=frame.m_width*2)
    {
        for (x=0; x<region.m_width && count<m_hpixelSize; x+=2, count++)
        {
            r = pixels[x];
            g1 = pixels[x - 1];
            g2 = pixels[-frame.m_width + x];
            b = pixels[-frame.m_width + x - 1];
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

    // if mean is too close to 0, the slope of the hue or sat lines will explode
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

#define GROW_INC                  4
#define GROW_MAX_DISTANCE         12.5
#define GROW_REGION_MAX_SIZE      100

int ColorLUT::growRegion(RectA *result, const Frame8 &frame, const Point16 &seed)
{
    uint8_t dir;
    Fpoint mean0, newMean;
    float dist;
    RectA newRegion, region;
    uint8_t done;

    m_hpixels = (HuePixel *)malloc(sizeof(HuePixel)*CL_HPIXEL_MAX_SIZE);
    if (m_hpixels==NULL)
        return -1; // not enough memory

    m_hpixelSize = CL_HPIXEL_MAX_SIZE;

    // create seed 2*GROW_INCx2*GROW_INC region from seed position, make sure it's within the frame
    region.m_xOffset = seed.m_x>GROW_INC ? seed.m_x-GROW_INC : 0;
    region.m_yOffset = seed.m_y>GROW_INC ? seed.m_y-GROW_INC : 0;
    region.m_width = 2*GROW_INC;
    if (region.m_xOffset+region.m_width>frame.m_width)
        region.m_width = frame.m_width-region.m_xOffset;
    region.m_height = 2*GROW_INC;
    if (region.m_yOffset+region.m_height>frame.m_height)
        region.m_height = frame.m_height-region.m_yOffset;

    map(frame, region);
    mean(&mean0);
    done = 0x00;

    while (1)
    {
        for (dir=0; dir<4; dir++)
        {
            if (done&(1<<dir))
                continue;
            else if (dir==0) // add to left
            {
                if (region.m_xOffset>GROW_INC)
                    newRegion.m_xOffset = region.m_xOffset-GROW_INC;
                else
                {
                    newRegion.m_xOffset = 0;
                    done |= 1<<dir;
                }
                newRegion.m_yOffset = region.m_yOffset;
                newRegion.m_width = GROW_INC;
                newRegion.m_height = region.m_height;
            }
            else if (dir==1) // add to top
            {
                if (region.m_yOffset>GROW_INC)
                    newRegion.m_yOffset = region.m_yOffset-GROW_INC;
                else
                {
                    newRegion.m_yOffset = 0;
                    done |= 1<<dir;
                }
                newRegion.m_xOffset = region.m_xOffset;
                newRegion.m_width = region.m_width;
                newRegion.m_height = GROW_INC;
            }
            else if (dir==2) // add to right
            {
                if (region.m_xOffset+region.m_width+GROW_INC>frame.m_width)
                {
                    newRegion.m_width = frame.m_width-region.m_xOffset-region.m_width;
                    done |= 1<<dir;
                }
                else
                    newRegion.m_width = GROW_INC;
                newRegion.m_xOffset = region.m_xOffset+region.m_width;
                newRegion.m_yOffset = region.m_yOffset;
                newRegion.m_height = region.m_height;
            }
            else // dir==3, add to bottom
            {
                if (region.m_yOffset+region.m_height+GROW_INC>frame.m_height)
                {
                    newRegion.m_height = frame.m_height-region.m_yOffset-region.m_height;
                    done |= 1<<dir;
                }
                else
                    newRegion.m_height = GROW_INC;
                newRegion.m_xOffset = region.m_xOffset;
                newRegion.m_yOffset = region.m_yOffset+region.m_height;
                newRegion.m_width = region.m_width;
            }

            // calculate new region mean
            map(frame, newRegion);
            mean(&newMean);

            // test new region
            dist = distance(mean0, newMean);
            qDebug() << dir << " "<< dist;

            if (dist>GROW_MAX_DISTANCE || m_hpixelLen==0)
                done |= 1<<dir;
            else // new region passes, so add new region
            {
                if (newRegion.m_xOffset<region.m_xOffset)
                {
                    region.m_xOffset = newRegion.m_xOffset;
                    region.m_width += newRegion.m_width;

                }
                else if (newRegion.m_yOffset<region.m_yOffset)
                {
                    region.m_yOffset = newRegion.m_yOffset;
                    region.m_height += newRegion.m_height;

                }
                else if (newRegion.m_xOffset+newRegion.m_width>region.m_xOffset+region.m_width)
                    region.m_width += newRegion.m_width;
                else if (newRegion.m_yOffset+newRegion.m_height>region.m_yOffset+region.m_height)
                    region.m_height += newRegion.m_height;
            }
            if (done==0x0f) // finished!
            {
                *result = region;
                free(m_hpixels);
                return 0;
            }
        }
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
