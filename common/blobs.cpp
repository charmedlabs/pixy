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
#ifdef PIXY
    m_maxCodedDist = MAX_CODED_DIST;
#else
    m_maxCodedDist = MAX_CODED_DIST/2;
#endif

    m_qq = qq;
    m_blobs = new uint16_t[m_maxBlobs*5];
    m_numBlobs = 0;
    m_blobReadIndex = 0;

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
	else
        m_maxBlobs = MAX_BLOBS;

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
    if (true)
    {
        m_codedBlobs = (BlobB *)(m_blobs + m_numBlobs*5);
        // calculate number of codedblobs left
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

int16_t Blobs::distance(BlobA *blob0, BlobA *blob1)
{
    int16_t left0, right0, top0, bottom0;
    int16_t left1, right1, top1, bottom1;

    left0 = blob0->m_left;
    right0 = blob0->m_right;
    top0 = blob0->m_top;
    bottom0 = blob0->m_bottom;
    left1 = blob1->m_left;
    right1 = blob1->m_right;
    top1 = blob1->m_top;
    bottom1 = blob1->m_bottom;

    if (left0>=left1 && ((top0<=top1 && top1<=bottom0) || (top0<=bottom1 && (bottom1<=bottom0 || top1<=top0))))
        return left0-right1;

    if (left1>=left0 && ((top0<=top1 && top1<=bottom0) || (top0<=bottom1 && (bottom1<=bottom0 || top1<=top0))))
        return left1-right0;

    if (top0>=top1 && ((left0<=left1 && left1<=right0) || (left0<=right1 && (right1<=right0 || left1<=left0))))
        return top0-bottom1;

    if (top1>=top0 && ((left0<=left1 && left1<=right0) || (left0<=right1 && (right1<=right0 || left1<=left0))))
        return top1-bottom0;

    return 0x7fff; // return a large number
}

bool Blobs::closeby(BlobA *blob0, BlobA *blob1)
{
    // check to see that the blobs are from color code models.  If they aren't both
    // color code blobs, we return false
#if 0
    if (m_clut->getType(blob0->m_model & ~0x07)!=CL_MODEL_TYPE_COLORCODE ||
            m_clut->getType(blob1->m_model & ~0x07)!=CL_MODEL_TYPE_COLORCODE)
        return false;
#endif

    // check to see if blobs are invalid or equal
    if (blob0->m_model==0 || blob1->m_model==0 || blob0->m_model==blob1->m_model)
        return false;

    return distance(blob0, blob1)<=m_maxCodedDist;
}

int16_t Blobs::distance(BlobA *blob0, BlobA *blob1, bool horiz)
{
    int16_t dist;

    if (horiz)
        dist = (blob0->m_right-blob0->m_left)/2 - (blob1->m_right-blob1->m_left)/2;
    else
        dist = (blob0->m_bottom-blob0->m_top)/2 - (blob1->m_bottom-blob1->m_top)/2;

    if (dist<0)
        return -dist;
    else
        return dist;
}

int16_t Blobs::angle(BlobA *blob0, BlobA *blob1)
{
    int acx, acy, bcx, bcy;
    float res;

    acx = (blob0->m_right + blob0->m_left)/2;
    acy = (blob0->m_bottom + blob0->m_top)/2;
    bcx = (blob1->m_right + blob1->m_left)/2;
    bcy = (blob1->m_bottom + blob1->m_top)/2;

    res = atan2((float)(acy-bcy), (float)(acx-bcx))*180/3.1415f;

    return (int16_t)res;
}

void Blobs::sort(BlobA *blobs[], uint16_t len, BlobA *firstBlob, bool horiz)
{
    uint16_t i, td, distances[MAX_COLOR_CODE_MODELS-1];
    bool done;
    BlobA *tb;

    // create list of distances
    for (i=0; i<len; i++)
        distances[i] = distance(firstBlob, blobs[i], horiz);

    // sort -- note, we only have 5 maximum to sort, so no worries about efficiency
    while(1)
    {
        for (i=1, done=true; i<len; i++)
        {
            if (distances[i-1]>distances[i])
            {
                // swap distances
                td = distances[i];
                distances[i] = distances[i-1];
                distances[i-1] = td;
                // swap blobs
                tb = blobs[i];
                blobs[i] = blobs[i-1];
                blobs[i-1] = tb;

                done = false;
            }
        }
        if (done)
            break;
    }
}

void Blobs::processCoded()
{
    uint16_t i, j, k, scount, count = 1;
    uint16_t left, right, top, bottom;
    uint16_t codedModel;
    BlobB *codedBlob;
    BlobA *blob0, *blob1, *endBlob;
    BlobA *blobs[MAX_COLOR_CODE_MODELS];

    endBlob = (BlobA *)m_blobs + m_numBlobs;

    // 1st pass:
    for (blob0=(BlobA *)m_blobs; blob0<endBlob; blob0++)
    {
        for (blob1=(BlobA *)m_blobs+1; blob1<endBlob; blob1++)
        {
            if (closeby(blob0, blob1))
            {
                qDebug("close");
                if (blob0->m_model<=NUM_MODELS && blob0->m_model<=NUM_MODELS)
                {
                    scount = count<<3;
                    blob0->m_model |= scount;
                    blob1->m_model |= scount;
                    count++;
                }
                else if (blob0->m_model>NUM_MODELS)
                {
                   scount = blob0->m_model & ~0x07;
                   blob1->m_model |= scount;
                }
                else if (blob1->m_model>NUM_MODELS)
                {
                   scount = blob1->m_model & ~0x07;
                   blob0->m_model |= scount;
                }
            }
        }
    }
    return;
    // 2nd pass
    for (i=1; i<count; i++)
    {
        scount = i<<3;
        // find all blobs with index i
        for (j=0, blob0=(BlobA *)m_blobs; blob0<endBlob; blob0++)
        {
            if ((blob0->m_model&scount)==scount)
            {
                blobs[j++] = blob0;
                if (j==MAX_COLOR_CODE_MODELS)
                    break;
            }
        }
        // find left, right, top, bottom of color coded block
        for (k=0, left=right=top=bottom=0; k<j; k++)
        {
            if (blobs[left]->m_left > blobs[k]->m_left)
                left = k;
            if (blobs[top]->m_top > blobs[k]->m_top)
                top = k;
            if (blobs[right]->m_right < blobs[k]->m_right)
                right = k;
            if (blobs[bottom]->m_bottom < blobs[k]->m_bottom)
                bottom = k;
        }
        // is it more horizontal than verticle?
        if (blobs[right]->m_right-blobs[left]->m_left > blobs[bottom]->m_bottom-blobs[top]->m_top)
            sort(blobs, j, blobs[left], true);
        else
            sort(blobs, j, blobs[top], false);

        // create new blob
        for (k=0, codedModel=0; k<j; k++, codedModel<<=3)
        {
            codedModel |= blobs[k]->m_model&0x07;
            blobs[k] = 0; // invalidate
        }
        codedBlob = m_codedBlobs + i - 1;
        codedBlob->m_model = codedModel;
        codedBlob->m_left = blobs[left]->m_left;
        codedBlob->m_right = blobs[right]->m_right;
        codedBlob->m_top = blobs[top]->m_top;
        codedBlob->m_bottom = blobs[bottom]->m_bottom;
        // calculate angle
        codedBlob->m_angle = angle(blobs[0], blobs[j-1]);

        m_numCodedBlobs++;
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


