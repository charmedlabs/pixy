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

#include "pixy_init.h"
#include "blobs.h"


Blobs::Blobs()
{
    int i;

    m_mutex = false;
    m_minArea = MIN_AREA;
    m_maxBlobs = MAX_BLOBS;
    m_maxBlobsPerModel = MAX_BLOBS_PER_MODEL;
    m_mergeDist = MAX_MERGE_DIST;
    m_maxBlob = NULL;
    m_maxCodedDist = MAX_CODED_DIST;
    m_blobs = new uint16_t[MAX_BLOBS*5];
    m_numBlobs = 0;
    m_blobReadIndex = 0;
    m_assembler.Reset();
}

Blobs::~Blobs()
{
    delete [] m_blobs;
}

int Blobs::handleSegment(uint16_t row, uint16_t startCol, uint16_t endCol)
{
    SSegment s;
    s.model = 1;
    s.row = row;
    s.startCol = startCol;
    s.endCol = endCol;
    return m_assembler.Add(s);
}

// Blob format:
// 0: model
// 1: left X edge
// 2: right X edge
// 3: top Y edge
// 4: bottom Y edge
int Blobs::runlengthAnalysis(Qqueue *qq)
{
    int32_t row = -1;
    int32_t icount = 0;
    Qval qval;
    int32_t res = 0;

    while (true)
    {
        // Wait for run-length calculations from M0
        while (qq->dequeue(&qval) == 0)
        { // Intentionally empty
        }

        // Break on end of frame or frame error
        if (qval.m_col_start >= QVAL_FRAME_ERROR)
            break;

        if (res < 0)
            continue;

        // Beginning of a new line marker
        if (qval.m_col_start == QVAL_LINE_BEGIN)
        {
            row++;
            if (icount++ == 5) // an interleave of every 5 lines or about every 175us seems good
            {
                g_chirpUsb->service();
                icount = 0;
            }
            continue;
        }

        res = handleSegment(row, qval.m_col_start, qval.m_col_end - 1);
    }

    m_assembler.EndFrame();
    m_assembler.SortFinished();

    if (qval.m_col_start==QVAL_FRAME_ERROR) // error code, queue overrun
        return -1;
    return 0;
}

int Blobs::blobify(Qqueue *qq)
{
    uint32_t i, j, k = 0;
    CBlob *blob;
    uint16_t *blobsStart;
    uint16_t numBlobsStart, invalid, invalid2;
    uint16_t left, top, right, bottom;
    //uint32_t timer, timer2=0;

    if (runlengthAnalysis(qq) < 0)
    {
        m_assembler.Reset();
        m_numBlobs = 0;
        return -1;
    }

    // copy blobs into memory
    invalid = 0;
    // mutex keeps interrupt routine from stepping on us
    m_mutex = true;

    m_maxBlob = NULL;
    m_numBlobs = 0;

    for (j=m_numBlobs*5, k=0, blobsStart=m_blobs+j, numBlobsStart=m_numBlobs, blob=m_assembler.finishedBlobs;
            blob && m_numBlobs<m_maxBlobs && k<m_maxBlobsPerModel; blob=blob->next, k++)
    {
        if (blob->GetArea()<(int)m_minArea)
            continue;
        blob->getBBox((short &)left, (short &)top, (short &)right, (short &)bottom);
        if (bottom-top<=1) // blobs that are 1 line tall
            continue;
        m_blobs[j + 0] = i+1;
        m_blobs[j + 1] = left;
        m_blobs[j + 2] = right;
        m_blobs[j + 3] = top;
        m_blobs[j + 4] = bottom;
        m_numBlobs++;
        j += 5;

    }
    //setTimer(&timer);
    while(1)
    {
        invalid2 = combine2(blobsStart, m_numBlobs-numBlobsStart);
        if (invalid2==0)
            break;
        invalid += invalid2;
    }
    //timer2 += getTimer(timer);

    //setTimer(&timer);
    invalid += combine(m_blobs, m_numBlobs);
    if (invalid)
    {
        invalid2 = compress(m_blobs, m_numBlobs);
        m_numBlobs -= invalid2;
    }
    //timer2 += getTimer(timer);
    //cprintf("time=%d\n", timer2); // never seen this greater than 200us.  or 1% of frame period

    // reset read indexes-- new frame
    m_blobReadIndex = 0;
    m_mutex = false;

    // free memory
    m_assembler.Reset();

    return 0;
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
        return 0;

    if (m_blobReadIndex==0) // beginning of frame, mark it with empty block
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


BlobA *Blobs::getMaxBlob(uint16_t signature, uint16_t *numBlobs)
{
    int i;
    uint16_t blobs;
    uint32_t area, maxArea;
    BlobA *blob, *maxBlob;

    if (m_mutex)
        return (BlobA *)-1;  // busy!

    if (signature==0) // 0 means return the biggest regardless of signature number
    {
        if (numBlobs)
            *numBlobs = 1; // not really used in this mode, so return 1

        // if we've already found it, return it
        if (m_maxBlob)
            return m_maxBlob;

        // look through all blobs looking for the blob with the biggest area
        for (i=0, maxArea=0; i<m_numBlobs; i++)
        {
            blob = (BlobA *)m_blobs + i;
            area = (blob->m_right - blob->m_left)*(blob->m_bottom - blob->m_top);
            if (area>maxArea)
            {
                maxArea = area;
                m_maxBlob = blob;
            }
        }
        return m_maxBlob;
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
    // check to see if blobs are invalid or equal
    if (blob0->m_model==0 || blob1->m_model==0 || blob0->m_model==blob1->m_model)
        return false;

    return distance(blob0, blob1)<=m_maxCodedDist;
}

int16_t Blobs::distance(BlobA *blob0, BlobA *blob1, bool horiz)
{
    int16_t dist;

    if (horiz)
        dist = (blob0->m_right+blob0->m_left)/2 - (blob1->m_right+blob1->m_left)/2;
    else
        dist = (blob0->m_bottom+blob0->m_top)/2 - (blob1->m_bottom+blob1->m_top)/2;

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

    res = atan2((float)(acy-bcy), (float)(bcx-acx))*180/3.1415f;

    return (int16_t)res;
}

void Blobs::sort(BlobA *blobs[], uint16_t len, BlobA *firstBlob, bool horiz)
{
    uint16_t i, td, distances[MAX_COLOR_CODE_MODELS*2];
    bool done;
    BlobA *tb;

    // create list of distances
    for (i=0; i<len && i<MAX_COLOR_CODE_MODELS*2; i++)
        distances[i] = distance(firstBlob, blobs[i], horiz);

    // sort -- note, we only have 5 maximum to sort, so no worries about efficiency
    while(1)
    {
        for (i=1, done=true; i<len && i<MAX_COLOR_CODE_MODELS*2; i++)
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

bool Blobs::analyzeDistances(BlobA *blobs0[], int16_t numBlobs0, BlobA *blobs[], int16_t numBlobs, BlobA **blobA, BlobA **blobB)
{
    bool skip;
    bool result = false;
    int16_t dist, minDist, i, j, k;

    for (i=0, minDist=0x7fff; i<numBlobs0; i++)
    {
        for (j=0; j<numBlobs; j++)
        {
            for (k=0, skip=false; k<numBlobs0; k++)
            {
                if (blobs0[k]==blobs[j] || (blobs0[k]->m_model&0x07)==(blobs[j]->m_model&0x07))
                {
                    skip = true;
                    break;
                }
            }
            if (skip)
                continue;
            dist = distance(blobs0[i], blobs[j]);
            if (dist<minDist)
            {
                minDist = dist;
                *blobA = blobs0[i];
                *blobB = blobs[j];
                result = true;
            }
        }
    }

    return result;
}

#define TOL  400

// impose weak size constraint
void Blobs::cleanup(BlobA *blobs[], int16_t *numBlobs)
{
    int i, j;
    bool set;
    uint16_t maxEqual, numEqual, numNewBlobs;
    BlobA *newBlobs[MAX_COLOR_CODE_MODELS*2];
    uint32_t area0, area1, lowerArea, upperArea, maxEqualArea;

    for (i=0, maxEqual=0, set=false; i<*numBlobs; i++)
    {
        area0 = (blobs[i]->m_right-blobs[i]->m_left) * (blobs[i]->m_bottom-blobs[i]->m_top);
        lowerArea = (area0*100)/(100+TOL);
        upperArea = area0 + (area0*TOL)/100;

        for (j=0, numEqual=0; j<*numBlobs; j++)
        {
            if (i==j)
                continue;
            area1 = (blobs[j]->m_right-blobs[j]->m_left) * (blobs[j]->m_bottom-blobs[j]->m_top);
            if (lowerArea<=area1 && area1<=upperArea)
                numEqual++;
        }
        if (numEqual>maxEqual)
        {
            maxEqual = numEqual;
            maxEqualArea = area0;
            set = true;
        }
    }

    if (!set)
        *numBlobs = 0;

    for (i=0, numNewBlobs=0; i<*numBlobs && numNewBlobs<MAX_COLOR_CODE_MODELS*2; i++)
    {
        area0 = (blobs[i]->m_right-blobs[i]->m_left) * (blobs[i]->m_bottom-blobs[i]->m_top);
        lowerArea = (area0*100)/(100+TOL);
        upperArea = area0 + (area0*TOL)/100;
        if (lowerArea<=maxEqualArea && maxEqualArea<=upperArea)
            newBlobs[numNewBlobs++] = blobs[i];
    }

    // copy new blobs over
    for (i=0; i<numNewBlobs; i++)
        blobs[i] = newBlobs[i];
    *numBlobs = numNewBlobs;
}


// eliminate duplicate and adjacent signatures
void Blobs::cleanup2(BlobA *blobs[], int16_t *numBlobs)
{
    BlobA *newBlobs[MAX_COLOR_CODE_MODELS*2];
    int i, j;
    uint16_t numNewBlobs;
    bool set;

    for (i=0, numNewBlobs=0, set=false; i<*numBlobs && numNewBlobs<MAX_COLOR_CODE_MODELS*2; i=j)
    {
        newBlobs[numNewBlobs++] = blobs[i];
        for (j=i+1; j<*numBlobs; j++)
        {
            if ((blobs[j]->m_model&0x07)==(blobs[i]->m_model&0x07))
                set = true;
            else
                break;
        }
    }
    if (set)
    {
        // copy new blobs over
        for (i=0; i<numNewBlobs; i++)
            blobs[i] = newBlobs[i];
        *numBlobs = numNewBlobs;
    }
}

void Blobs::printBlobs()
{
    int i;
    BlobA *blobs = (BlobA *)m_blobs;
    for (i=0; i<m_numBlobs; i++)
        printf("blob %d: %d %d %d %d %d", i, blobs[i].m_model, blobs[i].m_left, blobs[i].m_right, blobs[i].m_top, blobs[i].m_bottom);
}

void Blobs::mergeClumps(uint16_t scount0, uint16_t scount1)
{
    int i;
    BlobA *blobs = (BlobA *)m_blobs;
    for (i=0; i<m_numBlobs; i++)
    {
        if ((blobs[i].m_model&~0x07)==scount1)
            blobs[i].m_model = (blobs[i].m_model&0x07) | scount0;
    }
}
