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

#ifndef PIXY
#include "debug.h"
#else
#include "pixy_init.h"
#endif

#include "blobs.h"
#include <stdlib.h>  //added to get sort by area functionality

#define CC_SIGNATURE(s) (m_ccMode==CC_ONLY || m_clut.getType(s)==CL_MODEL_TYPE_COLORCODE)

Blobs::Blobs(Qqueue *qq, uint8_t *lut) : m_clut(lut)
{
    int i;

    m_mutex = false;
    m_minArea = MIN_AREA;
    m_maxBlobs = MAX_BLOBS;
    m_maxBlobsPerModel = MAX_BLOBS_PER_MODEL;
    m_mergeDist = MAX_MERGE_DIST;
    m_maxBlob = NULL;

	m_qq = qq;
#ifdef PIXY
    m_maxCodedDist = MAX_CODED_DIST;
#else
    m_maxCodedDist = MAX_CODED_DIST/2;
    m_qvals = new uint32_t[0x8000];
#endif
    m_ccMode = DISABLED;

    m_blobs = new uint16_t[MAX_BLOBS*5];
    m_ftcblobs = new uint16_t[MAX_FTCBLOBS*5];
    m_numBlobs = 0;
    m_blobReadIndex = 0;
    m_ccBlobReadIndex = 0;

    // reset blob assemblers
    for (i=0; i<CL_NUM_SIGNATURES; i++)
        m_assembler[i].Reset();
}

int Blobs::setParams(uint16_t maxBlobs, uint16_t maxBlobsPerModel, uint32_t minArea, ColorCodeMode ccMode)
{
    if (maxBlobs<=MAX_BLOBS)
        m_maxBlobs = maxBlobs;
    else
        m_maxBlobs = MAX_BLOBS;

    m_maxBlobsPerModel = maxBlobsPerModel;
    m_minArea = minArea;
    m_ccMode = ccMode;

    return 0;
}

Blobs::~Blobs()
{
    delete [] m_blobs;
#ifndef PIXY
    delete [] m_qvals;
#endif
}

int Blobs::handleSegment(uint8_t signature, uint16_t row, uint16_t startCol, uint16_t length)
{
	SSegment s;

    s.model = signature;
    s.row = row;
    s.startCol = startCol;
    s.endCol = startCol+length;

#ifndef PIXY
    uint32_t qval;

    qval = signature;
    qval |= startCol<<3;
    qval |= length<<12;

    m_qvals[m_numQvals++] = qval;
#endif

    return m_assembler[signature-1].Add(s);
}

// Blob format:
// 0: model
// 1: left X edge
// 2: right X edge
// 3: top Y edge
// 4: bottom Y edge
int Blobs::runlengthAnalysis()
{
    int32_t row=-1, icount=0;
    uint32_t startCol, sig, prevSig, prevStartCol, segmentStartCol, segmentEndCol, segmentSig=0;
    bool merge;
    Qval qval;
    int32_t res=0;
	register int32_t u, v, c;

#ifndef PIXY
    m_numQvals = 0;
#endif

    while(1)
    {
        while (m_qq->dequeue(&qval)==0);
        if (qval.m_col>=0xfffe)
            break;
		if (res<0)
			continue;
        if (qval.m_col==0)
        {
            prevStartCol = 0xffff;
            prevSig = 0;
            if (segmentSig)
            {
                res = handleSegment(segmentSig, row, segmentStartCol-1, segmentEndCol - segmentStartCol+1);
                segmentSig = 0;
            }
            row++;
#ifndef PIXY
            m_qvals[m_numQvals++] = 0;
#else
			if (icount++==5) // an interleave of every 5 lines or about every 175us seems good
			{
				g_chirpUsb->service();
				icount = 0;
			}
#endif
            continue;
        }

        sig = qval.m_col&0x07;

        u = qval.m_u;
        v = qval.m_v;

        u <<= CL_LUT_ENTRY_SCALE;
        v <<= CL_LUT_ENTRY_SCALE;
        c = qval.m_y;
        if (c==0)
            c = 1;
        u /= c;
        v /= c;

        if (m_clut.m_runtimeSigs[sig-1].m_uMin<u && u<m_clut.m_runtimeSigs[sig-1].m_uMax &&
                m_clut.m_runtimeSigs[sig-1].m_vMin<v && v<m_clut.m_runtimeSigs[sig-1].m_vMax && c>=(int32_t)m_clut.m_miny)
        {
         	qval.m_col >>= 3;
        	startCol = qval.m_col;
           	merge = startCol-prevStartCol<=5 && prevSig==sig;
            if (segmentSig==0 && merge)
            {
                segmentSig = sig;
                segmentStartCol = prevStartCol;
            }
            else if (segmentSig!=0 && (segmentSig!=sig || !merge))
            {
                res = handleSegment(segmentSig, row, segmentStartCol-1, segmentEndCol - segmentStartCol+1);
                segmentSig = 0;
            }

            if (segmentSig!=0 && merge)
                segmentEndCol = startCol;
            else if (segmentSig==0 && !merge)
                res = handleSegment(sig, row, startCol-1, 2);
            prevSig = sig;
            prevStartCol = startCol;
        }
        else if (segmentSig!=0)
        {
            res = handleSegment(segmentSig, row, segmentStartCol-1, segmentEndCol - segmentStartCol+1);
            segmentSig = 0;
        }
    }
	endFrame();

    if (qval.m_col==0xfffe) // error code, queue overrun
		return -1;
	return 0;
}

int Blobs::blobify()
{
    uint32_t i, j, k;
    bool colorCode;
    CBlob *blob;
    uint16_t *blobsStart;
    uint16_t numBlobsStart, invalid, invalid2;
    uint16_t left, top, right, bottom;
    //uint32_t timer, timer2=0;

	if (runlengthAnalysis()<0)
	{
   	 	for (i=0; i<CL_NUM_SIGNATURES; i++)
        	m_assembler[i].Reset();
    	m_numBlobs = 0;
		m_numCCBlobs = 0;
		return -1;
	}

    // copy blobs into memory
    invalid = 0;
    // mutex keeps interrupt routine from stepping on us
    m_mutex = true;

    m_maxBlob = NULL;

    for (i=0, m_numBlobs=0, m_numCCBlobs=0; i<CL_NUM_SIGNATURES; i++)
    {
        colorCode = CC_SIGNATURE(i+1);

        for (j=m_numBlobs*5, k=0, blobsStart=m_blobs+j, numBlobsStart=m_numBlobs, blob=m_assembler[i].finishedBlobs;
             blob && m_numBlobs<m_maxBlobs && k<m_maxBlobsPerModel; blob=blob->next, k++)
        {
            if ((colorCode && blob->GetArea()<MIN_COLOR_CODE_AREA) ||
                (!colorCode && blob->GetArea()<(int)m_minArea))
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
        if (!colorCode) // do not combine color code models
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
    if (m_ccMode!=DISABLED)
    {
        m_ccBlobs = (BlobB *)(m_blobs + m_numBlobs*5);
        // calculate number of codedblobs left
        processCC();
    }
    if (invalid || m_ccMode!=DISABLED)
    {
        invalid2 = compress(m_blobs, m_numBlobs);
        m_numBlobs -= invalid2;
    }
    //timer2 += getTimer(timer);
    //cprintf("time=%d\n", timer2); // never seen this greater than 200us.  or 1% of frame period

    // reset read indexes-- new frame
    m_blobReadIndex = 0;
    m_ccBlobReadIndex = 0;
    m_mutex = false;

    // free memory
    for (i=0; i<CL_NUM_SIGNATURES; i++)
        m_assembler[i].Reset();

#if 0
    static int frame = 0;
    if (m_numBlobs>0)
        cprintf("%d: blobs %d %d %d %d %d\n", frame, m_numBlobs, m_blobs[1], m_blobs[2], m_blobs[3], m_blobs[4]);
    else
        cprintf("%d: blobs 0\n", frame);
    frame++;
#endif
	return 0;
}

#ifndef PIXY
void Blobs::getRunlengths(uint32_t **qvals, uint32_t *len)
{
    *qvals = m_qvals;
    *len = m_numQvals;
}
#endif

uint16_t Blobs::getCCBlock(uint8_t *buf, uint32_t buflen)
{
    uint16_t *buf16 = (uint16_t *)buf;
    uint16_t temp, width, height;
    uint16_t checksum;
    uint16_t len = 8;  // default

    if (buflen<9*sizeof(uint16_t))
        return 0;

    if (m_mutex || m_ccBlobReadIndex>=m_numCCBlobs) // we're copying, so no CC blocks for now....
		return 0;

    if (m_blobReadIndex==0 && m_ccBlobReadIndex==0)	// beginning of frame, mark it with empty block
    {
        buf16[0] = BL_BEGIN_MARKER;
        len++;
        buf16++;
    }

    // beginning of block
    buf16[0] = BL_BEGIN_MARKER_CC;

    // model
    temp = m_ccBlobs[m_ccBlobReadIndex].m_model;
    checksum = temp;
    buf16[2] = temp;

    // width
    width = m_ccBlobs[m_ccBlobReadIndex].m_right - m_ccBlobs[m_ccBlobReadIndex].m_left;
    checksum += width;
    buf16[5] = width;

    // height
    height = m_ccBlobs[m_ccBlobReadIndex].m_bottom - m_ccBlobs[m_ccBlobReadIndex].m_top;
    checksum += height;
    buf16[6] = height;

    // x center
    temp = m_ccBlobs[m_ccBlobReadIndex].m_left + width/2;
    checksum += temp;
    buf16[3] = temp;

    // y center
    temp = m_ccBlobs[m_ccBlobReadIndex].m_top + height/2;
    checksum += temp;
    buf16[4] = temp;

    temp = m_ccBlobs[m_ccBlobReadIndex].m_angle;
    checksum += temp;
    buf16[7] = temp;

    buf16[1] = checksum;

    // next blob
    m_ccBlobReadIndex++;

    return len*sizeof(uint16_t);
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

    if (m_blobReadIndex>=m_numBlobs && m_ccMode!=DISABLED)
        return getCCBlock(buf, buflen);

    if (m_mutex || m_blobReadIndex>=m_numBlobs) // we're copying, so no blocks for now....
		return 0;

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


BlobA *Blobs::getMaxBlob(uint16_t signature, uint16_t *numBlobs)
{
    int i;
	uint16_t blobs;
    uint32_t area, maxArea;
    BlobA *blob, *maxBlob;
	BlobB *ccBlob;

	if (m_mutex)
		return (BlobA *)-1;	 // busy!

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
        for (i=0; i<m_numCCBlobs; i++)
        {
            ccBlob = (BlobB *)m_ccBlobs + i;
            area = (ccBlob->m_right - ccBlob->m_left)*(ccBlob->m_bottom - ccBlob->m_top);
            if (area>maxArea)
            {
                maxArea = area;
                m_maxBlob = (BlobA *)ccBlob;
            }
        }
		return m_maxBlob;
    }
	// look for a specific signature
    else
    {
		// regular signature
		if (signature<=CL_NUM_SIGNATURES)
		{
        	for (i=0, blobs=0, maxBlob=NULL; i<m_numBlobs; i++)
        	{
            	blob = (BlobA *)m_blobs + i;
           		if (blob->m_model==signature)
				{
					if (!maxBlob)
                		maxBlob = blob;
	 				blobs++;  // count
				}
        	}
		}
		// color code 
		else
		{
        	for (i=0, blobs=0, maxBlob=NULL; i<m_numCCBlobs; i++)
			{
            	ccBlob = (BlobB *)m_ccBlobs + i;
           		if (ccBlob->m_model==signature)
				{
					if (!maxBlob)
                		maxBlob = (BlobA *)ccBlob;
	 				blobs++;  // count
				}
			}
        }
		if (numBlobs)
			*numBlobs = blobs;
		return maxBlob;
    }

    return NULL; // no blobs...
}

static int compareBlobAreas(const void *a, const void *b)
{
	BlobA *ia = (struct BlobA *)a;
	BlobA *ib = (struct BlobA *)b;
	int areaA = (ia->m_right - ia->m_left)*(ia->m_bottom - ia->m_top);
	int areaB = (ib->m_right - ib->m_left)*(ib->m_bottom - ib->m_top);
	if (areaA < areaB)
	  return -1;
    else if (areaA > areaB)
	  return 1;
    else
	  return 0;

}

void Blobs::getMaxBlobs(uint16_t signature, uint16_t maxNumBlobs, BlobA **maxBlobs, uint16_t *numBlobs)
{
    int i;
	uint16_t blobs;
    BlobA *blob;
	BlobB *ccBlob;
	BlobA *ftcblob;

	if (m_mutex){
		*maxBlobs = (BlobA *)-1;
		*numBlobs =  0;
		return;
	}

    if (signature==0) // 0 means return the top maxNumBlobs largest area of and signature number <= 7
    {
    	//sort by Area
    	qsort(m_blobs, m_numBlobs, sizeof (BlobA), &compareBlobAreas);
		for (i=0; i<maxNumBlobs && i<m_numBlobs; i++)
		{
			blob = (BlobA *)m_blobs + i;
			ftcblob = (BlobA *)m_ftcblobs + i;
			ftcblob->m_model = blob->m_model;
			ftcblob->m_left = blob->m_left;
			ftcblob->m_right = blob->m_right;
			ftcblob->m_top = blob->m_top;
			ftcblob->m_bottom = blob->m_bottom;

		}

		*maxBlobs = (BlobA *)m_ftcblobs;
		*numBlobs =  m_numBlobs;
		return;
    }else if (signature==8) // 8 means return the top maxNumBlobs largest area of all cc signature number > 7
    {
    	//sort by Area
    	qsort(m_ccBlobs, m_numCCBlobs, sizeof (BlobB), &compareBlobAreas);
		for (i=0; i<maxNumBlobs && i<m_numCCBlobs; i++)
		{
			//ccBlob = (BlobB *)m_ccBlobs + i;
			blob = (BlobA *)m_ccBlobs + i;
			ftcblob = (BlobA *)m_ftcblobs + i;
			ftcblob->m_model = blob->m_model;
			ftcblob->m_left = blob->m_left;
			ftcblob->m_right = blob->m_right;
			ftcblob->m_top = blob->m_top;
			ftcblob->m_bottom = blob->m_bottom;
		}
		*maxBlobs = (BlobA *)m_ftcblobs;
		*numBlobs =  m_numCCBlobs;
		return;
    }
	// look for a specific signature
    else
    {
		// regular signatures
		if (signature<=CL_NUM_SIGNATURES)
		{
			qsort(m_blobs, m_numBlobs, sizeof (BlobA), &compareBlobAreas);
        	for (i=0, blobs=0; i<m_numBlobs; i++)
        	{
            	blob = (BlobA *)m_blobs + i;
           		if (blob->m_model==signature)
				{
					if (blobs < maxNumBlobs){
						ftcblob = (BlobA *)m_ftcblobs + i;
						ftcblob->m_model = blob->m_model;
						ftcblob->m_left = blob->m_left;
						ftcblob->m_right = blob->m_right;
						ftcblob->m_top = blob->m_top;
						ftcblob->m_bottom = blob->m_bottom;
					}
	 				blobs++;  // count
				}
        	}

		}
		// color code
		else
		{
			qsort(m_ccBlobs, m_numCCBlobs, sizeof (BlobB), &compareBlobAreas);
        	for (i=0, blobs=0; i<m_numCCBlobs; i++)
			{
            	ccBlob = (BlobB *)m_ccBlobs + i;
           		if (ccBlob->m_model==signature)
				{
           			if (blobs < maxNumBlobs){
           				ftcblob = (BlobA *)m_ftcblobs + i;
           				ftcblob->m_model = ccBlob->m_model;
           				ftcblob->m_left = ccBlob->m_left;
           				ftcblob->m_right = ccBlob->m_right;
           				ftcblob->m_top = ccBlob->m_top;
           				ftcblob->m_bottom = ccBlob->m_bottom;
           			}
	 				blobs++;  // count
				}
			}
        }
		*maxBlobs = (BlobA *)m_ftcblobs;
		*numBlobs = blobs;
		return;
    }

    *maxBlobs = (BlobA *)0;
    *numBlobs =  0;
    return;
}



void Blobs::getBlobs(BlobA **blobs, uint32_t *len, BlobB **ccBlobs, uint32_t *ccLen)
{
    *blobs = (BlobA *)m_blobs;
    *len = m_numBlobs;

    *ccBlobs = m_ccBlobs;
    *ccLen = m_numCCBlobs;
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
    // check to see that the blobs are from color code models.  If they aren't both
    // color code blobs, we return false
    if (!CC_SIGNATURE(blob0->m_model&0x07) || !CC_SIGNATURE(blob1->m_model&0x07))
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
#ifndef PIXY
    if (!result)
        DBG("not set!");
#endif
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
#ifndef PIXY
        else if (*numBlobs>=5 && (blobs[i]->m_model&0x07)==2)
            DBG("eliminated!");
#endif
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
#ifndef PIXY
    int i;
    BlobA *blobs = (BlobA *)m_blobs;
    for (i=0; i<m_numBlobs; i++)
        DBG("blob %d: %d %d %d %d %d", i, blobs[i].m_model, blobs[i].m_left, blobs[i].m_right, blobs[i].m_top, blobs[i].m_bottom);
#endif
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

void Blobs::processCC()
{
    int16_t i, j, k;
    uint16_t scount, scount1, count = 0;
    int16_t left, right, top, bottom;
    uint16_t codedModel0, codedModel;
    int32_t width, height, avgWidth, avgHeight;
    BlobB *codedBlob, *endBlobB;
    BlobA *blob0, *blob1, *endBlob;
    BlobA *blobs[MAX_COLOR_CODE_MODELS*2];

#if 0
    BlobA b0(1, 1, 20, 40, 50);
    BlobA b1(1, 1, 20, 52, 60);
    BlobA b2(1, 1, 20, 62, 70);
    BlobA b3(2, 22, 30, 40, 50);
    BlobA b4(2, 22, 30, 52, 60);
    BlobA b5(3, 32, 40, 40, 50);
    BlobA b6(4, 42, 50, 40, 50);
    BlobA b7(4, 42, 50, 52, 60);
    BlobA b8(6, 22, 30, 52, 60);
    BlobA b9(6, 22, 30, 52, 60);
    BlobA b10(7, 22, 30, 52, 60);

    BlobA *testBlobs[] =
    {
        &b0, &b1, &b2, &b3, &b4, &b5, &b6, &b7 //, &b8, &b9, &b10
    };
    int16_t ntb = 8;
    cleanup(testBlobs, &ntb);
#endif

    endBlob = (BlobA *)m_blobs + m_numBlobs;

    // 1st pass: mark all closeby blobs
    for (blob0=(BlobA *)m_blobs; blob0<endBlob; blob0++)
    {
        for (blob1=(BlobA *)blob0+1; blob1<endBlob; blob1++)
        {
            if (closeby(blob0, blob1))
            {
                if (blob0->m_model<=CL_NUM_SIGNATURES && blob1->m_model<=CL_NUM_SIGNATURES)
                {
                    count++;
                    scount = count<<3;
                    blob0->m_model |= scount;
                    blob1->m_model |= scount;
                }
                else if (blob0->m_model>CL_NUM_SIGNATURES && blob1->m_model<=CL_NUM_SIGNATURES)
                {
                    scount = blob0->m_model & ~0x07;
                    blob1->m_model |= scount;
                }
                else if (blob1->m_model>CL_NUM_SIGNATURES && blob0->m_model<=CL_NUM_SIGNATURES)
                {
                    scount = blob1->m_model & ~0x07;
                    blob0->m_model |= scount;
                }
            }
        }
    }

#if 1
    // 2nd pass: merge blob clumps
    for (blob0=(BlobA *)m_blobs; blob0<endBlob; blob0++)
    {
        if (blob0->m_model<=CL_NUM_SIGNATURES) // skip normal blobs
            continue;
        scount = blob0->m_model&~0x07;
        for (blob1=(BlobA *)blob0+1; blob1<endBlob; blob1++)
        {
            if (blob1->m_model<=CL_NUM_SIGNATURES)
                continue;

            scount1 = blob1->m_model&~0x07;
            if (scount!=scount1 && closeby(blob0, blob1))
                mergeClumps(scount, scount1);
        }
    }
#endif

    // 3rd and final pass, find each blob clean it up and add it to the table
    endBlobB = (BlobB *)((BlobA *)m_blobs + MAX_BLOBS)-1;
    for (i=1, codedBlob = m_ccBlobs, m_numCCBlobs=0; i<=count && codedBlob<endBlobB; i++)
    {
        scount = i<<3;
        // find all blobs with index i
        for (j=0, blob0=(BlobA *)m_blobs; blob0<endBlob && j<MAX_COLOR_CODE_MODELS*2; blob0++)
        {
            if ((blob0->m_model&~0x07)==scount)
                blobs[j++] = blob0;
        }

#if 1
        // cleanup blobs, deal with cases where there are more blobs than models
        cleanup(blobs, &j);
#endif

        if (j<2)
            continue;

        // find left, right, top, bottom of color coded block
        for (k=0, left=right=top=bottom=avgWidth=avgHeight=0; k<j; k++)
        {
            //DBG("* cc %x %d i %d: %d %d %d %d %d", blobs[k], m_numCCBlobs, k, blobs[k]->m_model, blobs[k]->m_left, blobs[k]->m_right, blobs[k]->m_top, blobs[k]->m_bottom);
            if (blobs[left]->m_left > blobs[k]->m_left)
                left = k;
            if (blobs[top]->m_top > blobs[k]->m_top)
                top = k;
            if (blobs[right]->m_right < blobs[k]->m_right)
                right = k;
            if (blobs[bottom]->m_bottom < blobs[k]->m_bottom)
                bottom = k;
            avgWidth += blobs[k]->m_right - blobs[k]->m_left;
            avgHeight += blobs[k]->m_bottom - blobs[k]->m_top;
        }
        avgWidth /= j;
        avgHeight /= j;
        codedBlob->m_left = blobs[left]->m_left;
        codedBlob->m_right = blobs[right]->m_right;
        codedBlob->m_top = blobs[top]->m_top;
        codedBlob->m_bottom = blobs[bottom]->m_bottom;

#if 1
        // is it more horizontal than vertical?
        width = (blobs[right]->m_right - blobs[left]->m_left)*100;
        width /= avgWidth; // scale by average width because our swatches might not be square
        height = (blobs[bottom]->m_bottom - blobs[top]->m_top)*100;
        height /= avgHeight; // scale by average height because our swatches might not be square

        if (width > height)
            sort(blobs, j, blobs[left], true);
        else
        	sort(blobs, j, blobs[top], false);

#if 1
        cleanup2(blobs, &j);
        if (j<2)
            continue;
        else if (j>5)
            j = 5;
#endif
        // create new blob, compare the coded models, pick the smaller one
        for (k=0, codedModel0=0; k<j; k++)
        {
            codedModel0 <<= 3;
            codedModel0 |= blobs[k]->m_model&0x07;
        }
        for (k=j-1, codedModel=0; k>=0; k--)
        {
            codedModel <<= 3;
            codedModel |= blobs[k]->m_model&0x07;
            blobs[k]->m_model = 0; // invalidate
        }

        if (codedModel0<codedModel)
        {
            codedBlob->m_model = codedModel0;
            codedBlob->m_angle = angle(blobs[0], blobs[j-1]);
        }
        else
        {
            codedBlob->m_model = codedModel;
            codedBlob->m_angle = angle(blobs[j-1], blobs[0]);
        }
#endif
        //DBG("cc %d %d %d %d %d", m_numCCBlobs, codedBlob->m_left, codedBlob->m_right, codedBlob->m_top, codedBlob->m_bottom);
        codedBlob++;
        m_numCCBlobs++;
    }

    // 3rd pass, invalidate blobs
    for (blob0=(BlobA *)m_blobs; blob0<endBlob; blob0++)
    {
        if (m_ccMode==MIXED)
        {
            if (blob0->m_model>CL_NUM_SIGNATURES)
                blob0->m_model = 0;
        }
        else if (blob0->m_model>CL_NUM_SIGNATURES || CC_SIGNATURE(blob0->m_model))
            blob0->m_model = 0; // invalidate-- not part of a color code
    }
}

void Blobs::endFrame()
{
    int i;
    for (i=0; i<CL_NUM_SIGNATURES; i++)
    {
        m_assembler[i].EndFrame();
        m_assembler[i].SortFinished();
    }
}

