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

#include <stdlib.h>
#include <math.h>
#include <string.h>
#ifndef PIXY
#include "debug.h"
#endif
#include "colorlut.h"
#include "calc.h"



IterPixel::IterPixel(const Frame8 &frame, const RectA &region)
{
    m_frame = frame;
    m_region = region;
    m_points = NULL;
    reset();
}

IterPixel::IterPixel(const Frame8 &frame, const Points *points)
{
    m_frame = frame;
    m_points = points;
    reset();
}

bool IterPixel::reset(bool cleari)
{
    if (cleari)
        m_i = 0;
    if (m_points)
    {
        if (m_points->size()>m_i)
        {
            m_region = RectA((*m_points)[m_i].m_x, (*m_points)[m_i].m_y, CL_GROW_INC, CL_GROW_INC);
            m_i++;
        }
        else
            return false; // empty!
    }
    m_x = m_y = 0;
    m_pixels = m_frame.m_pixels + (m_region.m_yOffset | 1)*m_frame.m_width + (m_region.m_xOffset | 1);
    return true;
}

bool IterPixel::next(UVPixel *uv, RGBPixel *rgb)
{
    if (m_points)
    {
        if (nextHelper(uv, rgb))
            return true; // working on the current block
        else // get new block
        {
            if (reset(false)) // reset indexes, increment m_i, get new block
                return nextHelper(uv, rgb);  // we have another block!
            else
                return false; // blocks are empty
        }
    }
    else
        return nextHelper(uv, rgb);
}


bool IterPixel::nextHelper(UVPixel *uv, RGBPixel *rgb)
{
    int32_t r, g1, g2, b, u, v, c, miny=CL_MIN_Y;

    while(1)
    {
        if (m_x>=m_region.m_width)
        {
            m_x = 0;
            m_y += 2;
            m_pixels += m_frame.m_width*2;
        }
        if (m_y>=m_region.m_height)
            return false;

        r = m_pixels[m_x];
        g1 = m_pixels[m_x - 1];
        g2 = m_pixels[-m_frame.m_width + m_x];
        b = m_pixels[-m_frame.m_width + m_x - 1];
		if (rgb)
		{
		  	rgb->m_r = r;
			rgb->m_g = (g1+g2)/2;
			rgb->m_b = b;
		}
		if (uv)
		{
        	c = r+g1+b;
            if (c<miny)
			{
				m_x += 2;
            	continue;
			}
        	u = ((r-g1)<<CL_LUT_ENTRY_SCALE)/c;
        	c = r+g2+b;
            if (c<miny)
			{
				m_x += 2;
            	continue;
			}
        	v = ((b-g2)<<CL_LUT_ENTRY_SCALE)/c;

        	uv->m_u = u;
        	uv->m_v = v;
		}

		m_x += 2;
        return true;
    }
}

uint32_t IterPixel::averageRgb(uint32_t *pixels)
{
	RGBPixel rgb;
	uint32_t r, g, b, n;
	reset();
	for (r=g=b=n=0; next(NULL, &rgb); n++)
	{
		r += rgb.m_r;
		g += rgb.m_g;
		b += rgb.m_b;		
	}

	r /= n;
	g /= n;
	b /= n;

	if (pixels)
		*pixels = n;
	return (r<<16) | (g<<8) | b;
}

ColorLUT::ColorLUT(uint8_t *lut)
{
	int i; 
    m_lut = lut;
    memset((void *)m_signatures, 0, sizeof(ColorSignature)*CL_NUM_SIGNATURES);
    memset((void *)m_runtimeSigs, 0, sizeof(RuntimeSignature)*CL_NUM_SIGNATURES);
	clearLUT();

    setMinBrightness(CL_DEFAULT_MINY);
    m_minRatio = CL_MIN_RATIO;
    m_maxDist = CL_MAX_DIST;
    m_ratio = CL_DEFAULT_TOL;
    m_ccGain = CL_DEFAULT_CCGAIN;
	for (i=0; i<CL_NUM_SIGNATURES; i++)
		m_sigRanges[i] = CL_DEFAULT_SIG_RANGE;
}


ColorLUT::~ColorLUT()
{
}

#if 0
void ColorLUT::calcRatios(IterPixel *ip, ColorSignature *sig, float ratios[])
{
    bool ubounded, vbounded;
    UVPixel uv;
    uint32_t un=0, vn=0, n=0, counts[4];
    longlong usum=0, vsum=0;
    counts[0] = counts[1] = counts[2] = counts[3] = 0;

    ip->reset();
    while(ip->next(&uv))
    {
        ubounded = true;
        vbounded = true;

        if (uv.m_u>sig->m_uMin)
            counts[0]++;
		else
            ubounded = false;

        if (uv.m_u<sig->m_uMax)
            counts[1]++;
		else
            ubounded = false;

        if (uv.m_v>sig->m_vMin)
            counts[2]++;
		else
            vbounded = false;

        if (uv.m_v<sig->m_vMax)
            counts[3]++;
		else
            vbounded = false;

        // only use pixels that are within our test bounds to form the mean
        if (ubounded)
        {
            usum += uv.m_u;
			un++;
		}
		if (vbounded)
		{
            vsum += uv.m_v;
            vn++;
        }
		n++;
    }

    // calc ratios
    ratios[0] = (float)counts[0]/n;
    ratios[1] = (float)counts[1]/n;
    ratios[2] = (float)counts[2]/n;
    ratios[3] = (float)counts[3]/n;
   // calc mean (because it's cheap to do it here)
    sig->m_uMean = usum/un;
    sig->m_vMean = vsum/vn;
 //	printf("%d %d %d %d %d %d %d %d %d\n", un, vn, n, sig->m_uMin, sig->m_uMax, sig->m_vMin, sig->m_vMax, sig->m_uMean, sig->m_vMean);
}

#else

void ColorLUT::calcRatios(IterPixel *ip, ColorSignature *sig, float ratios[])
{
    UVPixel uv;
    uint32_t n=0, counts[4];
    counts[0] = counts[1] = counts[2] = counts[3] = 0;

    ip->reset();
    while(ip->next(&uv))
    {
        if (uv.m_u>sig->m_uMin)
            counts[0]++;

        if (uv.m_u<sig->m_uMax)
            counts[1]++;

        if (uv.m_v>sig->m_vMin)
            counts[2]++;

        if (uv.m_v<sig->m_vMax)
            counts[3]++;

        n++;
    }

    // calc ratios
    ratios[0] = (float)counts[0]/n;
    ratios[1] = (float)counts[1]/n;
    ratios[2] = (float)counts[2]/n;
    ratios[3] = (float)counts[3]/n;
    // calc mean (because it's cheap to do it here)
    sig->m_uMean = (sig->m_uMin + sig->m_uMax)/2;
    sig->m_vMean = (sig->m_vMin + sig->m_vMax)/2;
}
#endif

void ColorLUT::iterate(IterPixel *ip, ColorSignature *sig)
{
    int32_t scale;
    float ratios[4];

    // binary search -- this rouine is guaranteed to find the right value +/- 1, which is good enough!
    // find all four values, umin, umax, vmin, vmax simultaneously
    for (scale=1<<30, sig->m_uMin=sig->m_uMax=sig->m_vMin=sig->m_vMax=0; scale!=0; scale>>=1)
    {
        calcRatios(ip, sig, ratios);
        if (ratios[0]>m_ratio)
            sig->m_uMin += scale;
        else
            sig->m_uMin -= scale;

        if (ratios[1]>m_ratio)
            sig->m_uMax -= scale;
        else
            sig->m_uMax += scale;

        if (ratios[2]>m_ratio)
            sig->m_vMin += scale;
        else
            sig->m_vMin -= scale;

        if (ratios[3]>m_ratio)
            sig->m_vMax -= scale;
        else
            sig->m_vMax += scale;
    }
}




int ColorLUT::generateSignature(const Frame8 &frame, const RectA &region, uint8_t signum)
{
 	if (signum<1 || signum>CL_NUM_SIGNATURES)
		return -1;
   // this is cool-- this routine doesn't allocate any extra memory other than some stack variables
    IterPixel ip(frame, region);
    iterate(&ip, m_signatures+signum-1);
	m_signatures[signum-1].m_type = 0;

	updateSignature(signum);
    return 0;
}


int ColorLUT::generateSignature(const Frame8 &frame, const Point16 &point, Points *points, uint8_t signum)
{
	if (signum<1 || signum>CL_NUM_SIGNATURES)
		return -1;
    // this routine requires some memory to store the region which consists of some consistently-sized blocks
    growRegion(frame, point, points);
    IterPixel ip(frame, points);
    iterate(&ip, m_signatures+signum-1);
	m_signatures[signum-1].m_type = 0;

	updateSignature(signum);
    return 0;
}

void ColorLUT::updateSignature(uint8_t signum)
{
    float range;

	if (signum<1 || signum>CL_NUM_SIGNATURES)
		return;
	signum--;

    if (m_signatures[signum].m_type==CL_MODEL_TYPE_COLORCODE)
        range = m_sigRanges[signum]*m_ccGain;
	else
		range = m_sigRanges[signum];
    m_runtimeSigs[signum].m_uMin = m_signatures[signum].m_uMean + (m_signatures[signum].m_uMin - m_signatures[signum].m_uMean)*range;
	m_runtimeSigs[signum].m_uMax = m_signatures[signum].m_uMean + (m_signatures[signum].m_uMax - m_signatures[signum].m_uMean)*range;
	m_runtimeSigs[signum].m_vMin = m_signatures[signum].m_vMean + (m_signatures[signum].m_vMin - m_signatures[signum].m_vMean)*range;
	m_runtimeSigs[signum].m_vMax = m_signatures[signum].m_vMean + (m_signatures[signum].m_vMax - m_signatures[signum].m_vMean)*range;

    m_runtimeSigs[signum].m_rgbSat = saturate(m_signatures[signum].m_rgb);
}

ColorSignature *ColorLUT::getSignature(uint8_t signum)
{
	if (signum<1 || signum>CL_NUM_SIGNATURES)
		return NULL;

	return m_signatures+signum-1;
}

int ColorLUT::setSignature(uint8_t signum, const ColorSignature &sig)
{
	if (signum<1 || signum>CL_NUM_SIGNATURES)
		return -1;

	m_signatures[signum-1] = sig;
	updateSignature(signum);
	return 0;
}


int ColorLUT::generateLUT()
{
    int32_t r, g, b, u, v, y, bin, sig;

    clearLUT();

    // recalc bounds for each signature
    for (r=0; r<CL_NUM_SIGNATURES; r++)
        updateSignature(r+1);

    for (r=0; r<1<<8; r+=1<<(8-CL_LUT_COMPONENT_SCALE))
    {
        for (g=0; g<1<<8; g+=1<<(8-CL_LUT_COMPONENT_SCALE))
        {
            for (b=0; b<1<<8; b+=1<<(8-CL_LUT_COMPONENT_SCALE))
            {
                y = r+g+b;

                if (y<(int32_t)m_miny)
                    continue;
                u = ((r-g)<<CL_LUT_ENTRY_SCALE)/y;
                v = ((b-g)<<CL_LUT_ENTRY_SCALE)/y;

                for (sig=0; sig<CL_NUM_SIGNATURES; sig++)
                {
                    if (m_signatures[sig].m_uMin==0 && m_signatures[sig].m_uMax==0)
                        continue;
                    if ((m_runtimeSigs[sig].m_uMin<u) && (u<m_runtimeSigs[sig].m_uMax) &&
                            (m_runtimeSigs[sig].m_vMin<v) && (v<m_runtimeSigs[sig].m_vMax))
                    {
                        u = r-g;
                        u >>= 9-CL_LUT_COMPONENT_SCALE;
                        u &= (1<<CL_LUT_COMPONENT_SCALE)-1;
                        v = b-g;
                        v >>= 9-CL_LUT_COMPONENT_SCALE;
                        v &= (1<<CL_LUT_COMPONENT_SCALE)-1;

                        bin = (u<<CL_LUT_COMPONENT_SCALE)+ v;

                        if (m_lut[bin]==0 || m_lut[bin]>sig+1)
                            m_lut[bin] = sig+1;
                    }
                }
            }
        }
    }

    return 0;
}


void ColorLUT::clearLUT(uint8_t signum)
{
    int i;

    for (i=0; i<CL_LUT_SIZE; i++)
    {
        if (signum==0)
            m_lut[i] = 0;
        else if (m_lut[i]==signum)
            m_lut[i] = 0;
    }
}


bool ColorLUT::growRegion(RectA *region, const Frame8 &frame, uint8_t dir)
{
    if (dir==0) // grow left
    {
        if (region->m_xOffset>=CL_GROW_INC)
        {
            region->m_xOffset -= CL_GROW_INC;
            region->m_width += CL_GROW_INC;
        }
        else
            return true;
    }
    else if (dir==1) // grow top
    {
        if (region->m_yOffset>=CL_GROW_INC)
        {
            region->m_yOffset -= CL_GROW_INC;
            region->m_height += CL_GROW_INC;
        }
        else
            return true;
    }
    else if (dir==2) // grow right
    {
        if (region->m_xOffset+region->m_width+CL_GROW_INC>frame.m_width)
            return true;
        region->m_width += CL_GROW_INC;
    }
    else if (dir==3) // grow bottom
    {
        if (region->m_yOffset+region->m_height+CL_GROW_INC>frame.m_height)
            return true;
        region->m_height += CL_GROW_INC;
    }
    return false;
}


float ColorLUT::testRegion(const RectA &region, const Frame8 &frame, UVPixel *mean, Points *points)
{
    UVPixel subMean;
    float distance;
    RectA subRegion(0, 0, CL_GROW_INC, CL_GROW_INC);
    subRegion.m_xOffset = region.m_xOffset;
    subRegion.m_yOffset = region.m_yOffset;
    bool horiz = region.m_width>region.m_height;
    uint32_t i, test, endpoint = horiz ? region.m_width : region.m_height;

    for (i=0, test=0; i<endpoint; i+=CL_GROW_INC)
    {
        getMean(subRegion, frame, &subMean);
        distance = sqrt((float)((mean->m_u-subMean.m_u)*(mean->m_u-subMean.m_u) + (mean->m_v-subMean.m_v)*(mean->m_v-subMean.m_v)));
        if ((uint32_t)distance<m_maxDist)
        {
            int32_t n = points->size();
            mean->m_u = ((longlong)mean->m_u*n + subMean.m_u)/(n+1);
            mean->m_v = ((longlong)mean->m_v*n + subMean.m_v)/(n+1);
            if (points->push_back(Point16(subRegion.m_xOffset, subRegion.m_yOffset))<0)
                break;
            //DBG("add %d %d %d", subRegion.m_xOffset, subRegion.m_yOffset, points->size());
            test++;
        }

        if (horiz)
            subRegion.m_xOffset += CL_GROW_INC;
        else
            subRegion.m_yOffset += CL_GROW_INC;
    }

    //DBG("return %f", (float)test*CL_GROW_INC/endpoint);
    return (float)test*CL_GROW_INC/endpoint;
}


void ColorLUT::growRegion(const Frame8 &frame, const Point16 &seed, Points *points)
{
    uint8_t dir, done;
    RectA region, newRegion;
    UVPixel mean;
    float ratio;

    done = 0;

    // create seed 2*CL_GROW_INCx2*CL_GROW_INC region from seed position, make sure it's within the frame
    region.m_xOffset = seed.m_x;
    region.m_yOffset = seed.m_y;
    if (growRegion(&region, frame, 0))
        done |= 1<<0;
    else
        points->push_back(Point16(region.m_xOffset, region.m_yOffset));
    if (growRegion(&region, frame, 1))
        done |= 1<<1;
    else
        points->push_back(Point16(region.m_xOffset, region.m_yOffset));
    if (growRegion(&region, frame, 2))
        done |= 1<<2;
    else
        points->push_back(Point16(seed.m_x, region.m_yOffset));
    if (growRegion(&region, frame, 3))
        done |= 1<<3;
    else
        points->push_back(seed);

    getMean(region, frame, &mean);

    while(done!=0x0f)
    {
        for (dir=0; dir<4; dir++)
        {
            newRegion = region;
            if (done&(1<<dir))
                continue;
            else if (dir==0) // left
                newRegion.m_width = 0;
            else if (dir==1) // top
                newRegion.m_height = 0; // top and bottom
            else if (dir==2) // right
            {
                newRegion.m_xOffset += newRegion.m_width;
                newRegion.m_width = 0;
            }
            else if (dir==3) // bottom
            {
                newRegion.m_yOffset += newRegion.m_height;
                newRegion.m_height = 0;
            }

            if (growRegion(&newRegion, frame, dir))
                done |= 1<<dir;
            else
            {
                ratio = testRegion(newRegion, frame, &mean, points);
                if (ratio<m_minRatio)
                    done |= 1<<dir;
                else
                    growRegion(&region, frame, dir);
            }
        }
    }
}


void ColorLUT::getMean(const RectA &region ,const Frame8 &frame, UVPixel *mean)
{
    UVPixel uv;
    uint32_t n=0;
    IterPixel ip(frame, region);

    longlong usum=0, vsum=0;

    while(ip.next(&uv))
    {
        usum += uv.m_u;
        vsum += uv.m_v;
        n++;
    }

    mean->m_u = usum/n;
    mean->m_v = vsum/n;
}

void ColorLUT::setSigRange(uint8_t signum, float range)
{
	if (signum<1 || signum>CL_NUM_SIGNATURES)
		return;
	m_sigRanges[signum-1] = range;
}

void ColorLUT::setGrowDist(uint32_t dist)
{
	m_maxDist = dist;
}

void ColorLUT::setMinBrightness(float miny)
{
    m_miny = 3*((1<<8)-1)*miny;
    if (m_miny==0)
        m_miny = 1;

}

void ColorLUT::setCCGain(float gain)
{
    m_ccGain = gain;
}

uint32_t ColorLUT::getType(uint8_t signum)
{
    if (signum<1 || signum>CL_NUM_SIGNATURES)
        return 0;

    return m_signatures[signum-1].m_type;
}

#if 0
uint32_t ColorLUT::getColor(uint8_t signum)
{
    int32_t r, g, b, max, u, v;

    if (signum<1 || signum>CL_NUM_SIGNATURES)
        return 0;

    u = m_signatures[signum-1].m_uMean;
    v = m_signatures[signum-1].m_vMean;

    // u = r-g
    // v = b-g
    if (abs(u)>abs(v))
    {
        if (u>0)
        {
            r = u;
            if (v>0)
                g = 0;
            else
                g = -v;
            b = v+g;
        }
        else
        {
            g = -u;
            r = 0;
            b = v+g;
        }
    }
    else
    {
        if (v>0)
        {
            b = v;
            if (u>0)
                g = 0;
            else
                g = -u;
            r = u+g;
        }
        else
        {
            g = -v;
            b = 0;
            r = u+g;
        }
    }

    if (r>g)
        max = r;
    else
        max = g;
    if (b>max)
        max = b;

    // normalize
    if (max>0)
    {
        r = (float)r/max*255;
        g = (float)g/max*255;
        b = (float)b/max*255;
        return (r<<16) | (g<<8) | b;
    }
    else
        return 0;
}
#endif
