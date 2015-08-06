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

    m_mutex = false;
    m_minArea = MIN_AREA;
    m_maxBlobs = MAX_BLOBS;
    m_maxBlobsPerModel = MAX_BLOBS_PER_MODEL;
    m_mergeDist = MAX_MERGE_DIST;
    m_maxCodedDist = MAX_CODED_DIST;

    m_qq = qq;
    m_blobs = new uint16_t[m_maxBlobs*5];
    m_numBlobs = 0;
    m_blobReadIndex = 0;
    m_endBlobs = m_blobs + m_maxBlobs*5 - 6;

#ifdef PIXY
    m_clut = new ColorLUT((void *)LUT_MEMORY);
#else
    m_lut = new uint8_t[CL_LUT_SIZE];
    m_clut = new ColorLUT(m_lut);
#endif

    // reset blob assemblers
    for (i=0; i<NUM_MODELS; i++)
        m_assembler[i].Reset();
}

int Blobs::setParams(uint16_t maxBlobs, uint16_t maxBlobsPerModel, uint32_t minArea)
{
    if (maxBlobs<=MAX_BLOBS)
        m_maxBlobs = maxBlobs;
    m_maxBlobsPerModel = maxBlobsPerModel;
    m_minArea = minArea;

    return 0;
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
    uint32_t i, j, k;
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
        for (j=m_numBlobs*5, k=0, blobsStart=m_blobs+j, numBlobsStart=m_numBlobs, blob=m_assembler[i].finishedBlobs;
             blob && m_numBlobs<m_maxBlobs && k<m_maxBlobsPerModel; blob=blob->next, k++)
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
    }
    //setTimer(&timer);
    invalid += combine(m_blobs, m_numBlobs);
    if (false)
    {
        m_codedBlobs = (BlobB *)(m_blobs + m_numBlobs*5);
        processCoded();
    }
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

bool Blobs::closeby(int a, int b)
{
    BlobA *blob0, *blob;
    int16_t left0, right0, top0, bottom0;
    int16_t left, right, top, bottom;

    blob0 = (BlobA *)m_blobs + a;
    blob = (BlobA *)m_blobs + b;

    if (blob0->m_model==0 || blob->m_model==0 || blob0->m_model==blob->m_model)
        return false;

    left0 = blob0->m_left;
    right0 = blob0->m_right;
    top0 = blob0->m_top;
    bottom0 = blob0->m_bottom;
    left = blob->m_left;
    right = blob->m_right;
    top = blob->m_top;
    bottom = blob->m_bottom;

    if (left0>=left && left0-right<=m_maxCodedDist &&
            ((top0<=top && top<=bottom0) || (top0<=bottom && (bottom<=bottom0 || top<=top0))))
        return true;

    if (left>=left0 && left-right0<=m_maxCodedDist &&
            ((top0<=top && top<=bottom0) || (top0<=bottom && (bottom<=bottom0 || top<=top0))))
        return true;

    if (top0>=top && top0-bottom<=m_maxCodedDist &&
            ((left0<=left && left<=right0) || (left0<=right && (right<=right0 || left<=left0))))
        return true;

    if (top>=top0 && top-bottom0<=m_maxCodedDist &&
            ((left0<=left && left<=right0) || (left0<=right && (right<=right0 || left<=left0))))
        return true;

    return false;
}

void Blobs::addCoded(int a, int b)
{
    uint16_t left, right, top, bottom;
    uint16_t codedModel;
    BlobA *blob0, *blob;
    BlobB *newBlob;

    blob0 = (BlobA *)m_blobs + a;
    blob = (BlobA *)m_blobs + b;

    codedModel = blob0->m_model;
    codedModel <<= 3;
    codedModel |= blob->m_model;

    left = blob0->m_left < blob->m_left ? blob0->m_left : blob->m_left;
    right = blob0->m_right > blob->m_right ? blob0->m_right : blob->m_right;
    top = blob0->m_top < blob->m_top ? blob0->m_top : blob->m_top;
    bottom = blob0->m_bottom > blob->m_bottom ? blob0->m_bottom : blob->m_bottom;

    // calculate angle
    int acx = (blob0->m_right + blob0->m_left)/2;
    int acy = (blob0->m_bottom + blob0->m_top)/2;
    int bcx = (blob->m_right + blob->m_left)/2;
    int bcy = (blob->m_bottom + blob->m_top)/2;
    float angle = atan2((float)(acy-bcy), (float)(acx-bcx))*180/3.1415;
    //qDebug() << "angle " << angle << " acx " << acx << " acy " << acy << " bcx " << bcx << " bcy " << bcy;

    // add rectangle
    newBlob = m_codedBlobs + m_numCodedBlobs;
    if (newBlob>(BlobB *)m_endBlobs)
        return;

    newBlob->m_model = codedModel;
    newBlob->m_left = left;
    newBlob->m_right = right;
    newBlob->m_top = top;
    newBlob->m_bottom = bottom;
    newBlob->m_angle = (int16_t)angle;

    m_numCodedBlobs++;

    // invalidate a and b
    blob0->m_model = 0;
    blob->m_model = 0;
}

void Blobs::processCoded()
{
    int i, j;

    m_numCodedBlobs = 0;
    for (i=0; i<m_numBlobs; i++)
    {
        for (j=i+1; j<m_numBlobs; j++)
        {
            if (closeby(i, j))
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


