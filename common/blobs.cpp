#ifdef PIXY
#include "pixy_init.h"
#include "misc.h"
#else
#include "pixymon.h"
#endif
#include "blobs.h"
#include "colorlut.h"


Blobs::Blobs(Qqueue *qq)
{
    int i;

    m_qq = qq;
    m_blobs = new uint16_t[MAX_BLOBS*5];
    m_numBlobs = 0;
    m_blobReadIndex = 0;

#ifdef PIXY
    m_clut = new ColorLUT((void *)LUT_MEMORY);
#else
    m_lut = new uint8_t[CL_LUT_SIZE];
    m_clut = new ColorLUT(m_lut);
#endif

    m_mutex = false;
    m_minArea = MIN_AREA;
    m_mergeDist = MAX_MERGE_DIST;

    // reset blob assemblers
    for (i=0; i<NUM_MODELS; i++)
        m_assembler[i].Reset();
}


Blobs::~Blobs()
{
#ifndef PIXY
    delete [] m_lut;
#endif
    delete m_clut;
    delete [] m_blobs;
}

// Blob format:
// 0: model
// 1: left X edge
// 2: right X edge
// 3: top Y edge
// 4: bottom Y edge

void Blobs::blobify()
{
    uint32_t i, j;
    CBlob *blob;
    uint16_t *blobsStart;
    uint16_t numBlobsStart, invalid, invalid2;
    uint16_t left, top, right, bottom;
    //uint32_t timer, timer2=0;

    unpack();

    // copy blobs into memory
    invalid = 0;
    // mutex keeps interrupt routine from stepping on us
    m_mutex = true;
    for (i=0, m_numBlobs=0; i<NUM_MODELS; i++)
    {
        for (j=m_numBlobs*5, blobsStart=m_blobs+j, numBlobsStart=m_numBlobs, blob=m_assembler[i].finishedBlobs;
             blob && m_numBlobs<MAX_BLOBS; blob=blob->next)
        {
            if (blob->GetArea()<(int)m_minArea)
                continue;
            blob->getBBox((short &)left, (short &)top, (short &)right, (short &)bottom);
            m_blobs[j + 0] = i+1;
            m_blobs[j + 1] = left;
            m_blobs[j + 2] = right;
            m_blobs[j + 3] = top;
            m_blobs[j + 4] = bottom;
            m_numBlobs++;
            j += 5;

        }
#if 1
        //setTimer(&timer);
        if (true)
        {
            while(1)
            {
                invalid2 = combine2(blobsStart, m_numBlobs-numBlobsStart);
                if (invalid2==0)
                    break;
                invalid += invalid2;
            }
        }
        //timer2 += getTimer(timer);
#endif
    }
    //setTimer(&timer);
    invalid += combine(m_blobs, m_numBlobs);
    if (invalid)
    {
        invalid2 = compress(m_blobs, m_numBlobs);
        m_numBlobs -= invalid2;
        if (invalid2!=invalid)
            cprintf("**** %d %d\n", invalid2, invalid);

    }
    //timer2 += getTimer(timer);
    //cprintf("time=%d\n", timer2); // never seen this greater than 200us.  or 1% of frame period

    // reset read index-- new frame
    m_blobReadIndex = 0;
    m_mutex = false;

    // free memory
    for (i=0; i<NUM_MODELS; i++)
        m_assembler[i].Reset();

#if 0
    static int frame = 0;
    if (m_numBlobs>0)
        cprintf("%d: blobs %d %d %d %d %d\n", frame, m_numBlobs, m_blobs[1], m_blobs[2], m_blobs[3], m_blobs[4]);
    else
        cprintf("%d: blobs 0\n", frame);
    frame++;
#endif
}

void Blobs::unpack()
{
    SSegment s;
    int32_t row;
    bool memfull;
    uint32_t i;
    Qval qval;

    // q val:
    // | 4 bits    | 7 bits      | 9 bits | 9 bits    | 3 bits |
    // | shift val | shifted sum | length | begin col | model  |

    row = -1;
    memfull = false;
    i = 0;

    while(1)
    {
        while (m_qq->dequeue(&qval)==0);
        if (qval==0xffffffff)
            break;
        i++;
        if (qval==0)
        {
            row++;
            continue;
        }
        s.model = qval&0x07;
        if (s.model>0 && !memfull)
        {
            s.row = row;
            qval >>= 3;
            s.startCol = qval&0x1ff;
            qval >>= 9;
            s.endCol = (qval&0x1ff) + s.startCol;
            if (m_assembler[s.model-1].Add(s)<0)
            {
                memfull = true;
                cprintf("heap full %d\n", i);
            }
        }
    }
    //cprintf("rows %d %d\n", row, i);
    // finish frame
    for (i=0; i<NUM_MODELS; i++)
    {
        m_assembler[i].EndFrame();
        m_assembler[i].SortFinished();
    }
}

uint16_t Blobs::getBlock(uint8_t *buf, uint32_t buflen)
{							
	uint16_t *buf16 = (uint16_t *)buf;
    uint16_t temp, width, height;
    uint16_t checksum;
    uint16_t len = 7;  // default
    int i = m_blobReadIndex*5;

	if (buflen<8*sizeof(uint16_t))
		return 0;

    if (m_mutex || m_blobReadIndex>=m_numBlobs) // we're copying, so no blocks for now....
    {	// return a couple null words to give us time to copy
        // (otherwise we may spend too much time in the ISR)
        buf16[0] = 0;
        buf16[1] = 0;
        return 2;
    }

    if (m_blobReadIndex==0)	// beginning of frame, mark it with empty block
    {
        buf16[0] = BL_BEGIN_MARKER;
        len++;
        buf16++;
    }

    // beginning of block
    buf16[0] = BL_BEGIN_MARKER;

    // model
    temp = m_blobs[i];
    checksum = temp;
    buf16[2] = temp;

    // width
    width = m_blobs[i+2] - m_blobs[i+1];
    checksum += width;
    buf16[5] = width;

    // height
    height = m_blobs[i+4] - m_blobs[i+3];
    checksum += height;
    buf16[6] = height;

    // x center
    temp = m_blobs[i+1] + width/2;
    checksum += temp;
    buf16[3] = temp;

    // y center
    temp = m_blobs[i+3] + height/2;
    checksum += temp;
    buf16[4] = temp;

    buf16[1] = checksum;

    // next blob
    m_blobReadIndex++;

    return len*sizeof(uint16_t);
}


uint16_t *Blobs::getMaxBlob(uint16_t signature)
{
    int i, j;

	if (signature==0) // 0 means ignore signature
	{
		if (m_numBlobs>0)
			return m_blobs; // return first blob regardless of signature
	}
	else
	{
    	for (i=0, j=0; i<m_numBlobs; i++, j+=5)
    	{
       		if (m_blobs[j+0]==signature)
            	return m_blobs+j;
    	}
	}

    return NULL; // no blobs...
} 

void Blobs::getBlobs(BlobA **blobs, uint32_t *len)
{
    *blobs = (BlobA *)m_blobs;
    *len = m_numBlobs;
}



uint16_t Blobs::compress(uint16_t *blobs, uint16_t numBlobs)
{
    uint16_t i, ii;
    uint16_t *destination, invalid;

    // compress list
    for (i=0, ii=0, destination=NULL, invalid=0; i<numBlobs; i++, ii+=5)
    {
        if (blobs[ii+0]==0)
        {
            if (destination==NULL)
                destination = blobs+ii;
            invalid++;
            continue;
        }
        if (destination)
        {
            destination[0] = blobs[ii+0];
            destination[1] = blobs[ii+1];
            destination[2] = blobs[ii+2];
            destination[3] = blobs[ii+3];
            destination[4] = blobs[ii+4];
            destination += 5;
        }
    }
    return invalid;
}

uint16_t Blobs::combine(uint16_t *blobs, uint16_t numBlobs)
{
    uint16_t i, j, ii, jj, left0, right0, top0, bottom0;
    uint16_t left, right, top, bottom;
    uint16_t invalid;

    // delete blobs that are fully enclosed by larger blobs
    for (i=0, ii=0, invalid=0; i<numBlobs; i++, ii+=5)
    {
        if (blobs[ii+0]==0)
            continue;
        left0 = blobs[ii+1];
        right0 = blobs[ii+2];
        top0 = blobs[ii+3];
        bottom0 = blobs[ii+4];

        for (j=i+1, jj=ii+5; j<numBlobs; j++, jj+=5)
        {
            if (blobs[jj+0]==0)
                continue;
            left = blobs[jj+1];
            right = blobs[jj+2];
            top = blobs[jj+3];
            bottom = blobs[jj+4];

            if (left0<=left && right0>=right && top0<=top && bottom0>=bottom)
            {
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (left<=left0 && right>=right0 && top<=top0 && bottom>=bottom0)
            {
                blobs[ii+0] = 0; // invalidate
                invalid++;
            }

        }
    }

    return invalid;
}

uint16_t Blobs::combine2(uint16_t *blobs, uint16_t numBlobs)
{
    uint16_t i, j, ii, jj, left0, right0, top0, bottom0;
    uint16_t left, right, top, bottom;
    uint16_t invalid;

    // delete blobs that are fully enclosed by larger blobs
    for (i=0, ii=0, invalid=0; i<numBlobs; i++, ii+=5)
    {
        if (blobs[ii+0]==0)
            continue;
        left0 = blobs[ii+1];
        right0 = blobs[ii+2];
        top0 = blobs[ii+3];
        bottom0 = blobs[ii+4];

        for (j=i+1, jj=ii+5; j<numBlobs; j++, jj+=5)
        {
            if (blobs[jj+0]==0)
                continue;
            left = blobs[jj+1];
            right = blobs[jj+2];
            top = blobs[jj+3];
            bottom = blobs[jj+4];

#if 1 // if corners touch....
            if (left<=left0 && left0-right<=m_mergeDist &&
                    ((top0<=top && top<=bottom0) || (top0<=bottom && bottom<=bottom0)))
            {
                blobs[ii+1] = left;
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (right>=right0 && left-right0<=m_mergeDist &&
                     ((top0<=top && top<=bottom0) || (top0<=bottom && bottom<=bottom0)))
            {
                blobs[ii+2] = right;
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (top<=top0 && top0-bottom<=m_mergeDist &&
                     ((left0<=left && left<=right0) || (left0<=right && right<=right0)))
            {
                blobs[ii+3] = top;
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (bottom>=bottom0 && top-bottom0<=m_mergeDist &&
                     ((left0<=left && left<=right0) || (left0<=right && right<=right0)))
            {
                blobs[ii+4] = bottom;
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
#else // at least half of a side (the smaller adjacent side) has to overlap
            if (left<=left0 && left0-right<=m_mergeDist &&
                    ((top<=top0 && top0<=top+height) || (top+height<=bottom0 && bottom0<=bottom)))
            {
                blobs[ii+1] = left;
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (right>=right0 && left-right0<=m_mergeDist &&
                     ((top<=top0 && top0<=top+height) || (top+height<=bottom0 && bottom0<=bottom)))
            {
                blobs[ii+2] = right;
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (top<=top0 && top0-bottom<=m_mergeDist &&
                     ((left<=left0 && left0<=left+width) || (left+width<=right0 && right0<=right)))
            {
                blobs[ii+3] = top;
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (bottom>=bottom0 && top-bottom0<=m_mergeDist &&
                     ((left<=left0 && left0<=left+width) || (left+width<=right0 && right0<=right)))
            {
                blobs[ii+4] = bottom;
                blobs[jj+0] = 0; // invalidate
                invalid++;
            }
#endif
        }
    }

    return invalid;
}

#define MAX_CODED_DIST  10

bool Blobs::closeby(int a, int b, int dist)
{

    int16_t left0, right0, top0, bottom0;
    int16_t left, right, top, bottom;
    uint8_t model0, model;

    left0 = m_blobs[a*4+0];
    if (left0==0)
        return false;
    model0 = left0&0x07;
    left0 >>= 3;
    right0 = m_blobs[a*4+1]>>3;
    top0 = m_blobs[a*4+2]>>3;
    bottom0 = m_blobs[a*4+3]>>3;

    left = m_blobs[b*4+0];
    if (left==0)
        return false;
    model = left&0x07;
    if (model0==model)
        return false;
    left >>= 3;
    right = m_blobs[b*4+1]>>3;
    top = m_blobs[b*4+2]>>3;
    bottom = m_blobs[b*4+3]>>3;

    if (left0>=left && left0-right<=dist &&
            ((top0<=top && top<=bottom0) || (top0<=bottom && (bottom<=bottom0 || top<=top0))))
        return true;

    if (left>=left0 && left-right0<=dist &&
            ((top0<=top && top<=bottom0) || (top0<=bottom && (bottom<=bottom0 || top<=top0))))
        return true;

    if (top0>=top && top0-bottom<=dist &&
            ((left0<=left && left<=right0) || (left0<=right && (right<=right0 || left<=left0))))
        return true;

    if (top>=top0 && top-bottom0<=dist &&
            ((left0<=left && left<=right0) || (left0<=right && (right<=right0 || left<=left0))))
        return true;

    return false;
}

void Blobs::addCoded(int a, int b)
{
    //static int i=0;
    uint16_t left, right, top, bottom;
    uint8_t model;
    uint16_t codedModel;

    model = m_blobs[a*4]&0x07;
    codedModel = model;
    codedModel <<= 3;
    model = m_blobs[b*4]&0x07;
    codedModel |= model;
    codedModel <<= 3; // 0 escape

    left = m_blobs[a*4+0]>>3 < m_blobs[b*4+0]>>3 ? m_blobs[a*4+0]>>3 : m_blobs[b*4+0]>>3;
    right = m_blobs[a*4+1]>>3 > m_blobs[b*4+1]>>3 ? m_blobs[a*4+1]>>3 : m_blobs[b*4+1]>>3;
    top = m_blobs[a*4+2]>>3 < m_blobs[b*4+2]>>3 ? m_blobs[a*4+2]>>3 : m_blobs[b*4+2]>>3;
    bottom = m_blobs[a*4+3]>>3 > m_blobs[b*4+3]>>3 ? m_blobs[a*4+3]>>3 : m_blobs[b*4+3]>>3;

#ifdef RENDER_ANGLE
    // calculate angle
    int acx = ((m_blobs[a*4+1]>>3)+(m_blobs[a*4+0]>>3))/2;
    int acy = ((m_blobs[a*4+3]>>3)+(m_blobs[a*4+2]>>3))/2;
    int bcx = ((m_blobs[b*4+1]>>3)+(m_blobs[b*4+0]>>3))/2;
    int bcy = ((m_blobs[b*4+3]>>3)+(m_blobs[b*4+2]>>3))/2;
    double angle = atan2(acy-bcy, acx-bcx)*180/3.1415;
    //qDebug() << "angle " << angle << " acx " << acx << " acy " << acy << " bcx " << bcx << " bcy " << bcy;

    // add rectangle
    m_blobs[m_numBlobs*4+m_numCodedBlobs*6+0] = codedModel;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*6+1] = left;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*6+2] = right;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*6+3] = top;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*6+4] = bottom;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*6+5] = (int16_t)angle;
#else
    // add rectangle
    m_blobs[m_numBlobs*4+m_numCodedBlobs*5+0] = codedModel;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*5+1] = left;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*5+2] = right;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*5+3] = top;
    m_blobs[m_numBlobs*4+m_numCodedBlobs*5+4] = bottom;
#endif

    m_numCodedBlobs++;

    // invalidate a and b (only if you are
    m_blobs[a*4+0] = 0;
    m_blobs[a*4+1] = 0;
    m_blobs[a*4+2] = 0;
    m_blobs[a*4+3] = 0;
    m_blobs[b*4+0] = 0;
    m_blobs[b*4+1] = 0;
    m_blobs[b*4+2] = 0;
    m_blobs[b*4+3] = 0;
}

void Blobs::processCoded()
{
    int i, j;

    m_numCodedBlobs = 0;
    for (i=0; i<m_numBlobs; i++)
    {
        for (j=i+1; j<m_numBlobs; j++)
        {
            if (closeby(i, j, MAX_CODED_DIST))
                addCoded(i, j);
        }
    }
}

int Blobs::generateLUT(uint8_t model, const Frame8 &frame, const RectA &region, ColorModel *pcmodel)
{
    int goodness;
    ColorModel cmodel;
    if (model>NUM_MODELS)
        return -1;

    goodness = m_clut->generate(&cmodel, frame, region);
    if (goodness==0)
        return -1; // this model sucks!
    m_clut->clear(model);
    m_clut->add(&cmodel, model);

    if (pcmodel)
        *pcmodel = cmodel;

    return goodness;
}

int Blobs::generateLUT(uint8_t model, const Frame8 &frame, const Point16 &seed, ColorModel *pcmodel, RectA *region)
{
    int goodness;
    RectA cregion;
    ColorModel cmodel;

    m_clut->growRegion(&cregion, frame, seed);

    goodness = m_clut->generate(&cmodel, frame, cregion);
    if (goodness==0)
        return -1; // this model sucks!

    m_clut->clear(model);
    m_clut->add(&cmodel, model);

    if (region)
        *region = cregion;

    if (pcmodel)
        *pcmodel = cmodel;

    return goodness;
}


