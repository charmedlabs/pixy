#include "pixy_init.h"
#include "blobs.h"
#include "qqueue.h"


Blobs::Blobs(Qqueue *qq)
{
	m_qq = qq;
    m_boxes = new uint16_t[MAX_BLOBS*5];
    m_minArea = MIN_AREA;
    m_mergeDist = MAX_MERGE_DIST;
}


Blobs::~Blobs()
{
    delete [] m_boxes;
}

void Blobs::blobify()
{
    SSegment s;
    int32_t row;
    Qval qval;
	uint32_t i, j;
    CBlob *blob;
    uint16_t *boxesStart;
    uint16_t numBoxesStart, invalid, invalid2;
    uint16_t left, top, right, bottom;

    // q val:
    // | 4 bits    | 7 bits      | 9 bits | 9 bits    | 3 bits |
    // | shift val | shifted sum | length | begin col | model  |

    // start frame
    for (i=0; i<NUM_MODELS; i++)
        m_assembler[i].Reset();

   	row = -1;
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
        if (s.model>0)
        {
            s.row = row;
            qval >>= 3;
            s.startCol = qval&0x1ff;
            qval >>= 9;
            s.endCol = (qval&0x1ff) + s.startCol;
            if (m_assembler[s.model-1].Add(s)<0)
                break;
        }
    }
	cprintf("rows %d %d\n", row, i);


    for (i=0, m_numBoxes=0; i<NUM_MODELS; i++)
    {
        m_assembler[i].EndFrame();
        m_assembler[i].SortFinished();

        for (j=m_numBoxes*5, boxesStart=m_boxes+j, numBoxesStart=m_numBoxes, blob=m_assembler[i].finishedBlobs;
             blob; blob=blob->next)
        {
            if (blob->GetArea()<(int)m_minArea)
                continue;
            blob->getBBox((short &)left, (short &)top, (short &)right, (short &)bottom);
            // shift by 4 because we need to double the parameters (we're processing at 1/4 resolution)
            m_boxes[j + 0] = i+1;
            m_boxes[j + 1] = left<<1;
            m_boxes[j + 2] = right<<1;
            m_boxes[j + 3] = top<<1;
            m_boxes[j + 4] = bottom<<1;
            m_numBoxes++;
            j += 5;

        }
#if 0
        invalid = combine(boxesStart, m_numBoxes-numBoxesStart);
        if (1)
        {
            while ((invalid2=combine2(boxesStart, m_numBoxes-numBoxesStart)))
                invalid += invalid2;
        }
        if (invalid)
        {
            invalid2 = compress(boxesStart, m_numBoxes-numBoxesStart);
            m_numBoxes -= invalid2;
        }
#endif
    }
}

uint16_t Blobs::compress(uint16_t *boxes, uint16_t numBoxes)
{
    uint16_t i, ii;
    uint16_t *destination, invalid;

    // compress list
    for (i=0, ii=0, destination=NULL, invalid=0; i<numBoxes; i++, ii+=5)
    {
        if (boxes[ii+0]==0)
        {
            if (destination==NULL)
                destination = boxes+ii;
            invalid++;
            continue;
        }
        if (destination)
        {
            destination[0] = boxes[ii+0];
            destination[1] = boxes[ii+1];
            destination[2] = boxes[ii+2];
            destination[3] = boxes[ii+3];
            destination[4] = boxes[ii+4];
            destination += 5;
        }
    }
    return invalid;
}

uint16_t Blobs::combine(uint16_t *boxes, uint16_t numBoxes)
{
    uint16_t i, j, ii, jj, left0, right0, top0, bottom0;
    uint16_t left, right, top, bottom;
    uint16_t invalid;

    // delete blobs that are fully enclosed by larger blobs
    for (i=0, ii=0, invalid=0; i<numBoxes; i++, ii+=5)
    {
        if (boxes[ii+0]==0)
            continue;
        left0 = boxes[ii+1];
        right0 = boxes[ii+2];
        top0 = boxes[ii+3];
        bottom0 = boxes[ii+4];

        for (j=i+1, jj=ii+5; j<numBoxes; j++, jj+=5)
        {
            if (boxes[jj+0]==0)
                continue;
            left = boxes[jj+1];
            right = boxes[jj+2];
            top = boxes[jj+3];
            bottom = boxes[jj+4];

            if (left0<=left && right0>=right &&
                    top0<=top && bottom0>=bottom)
            {
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
        }
    }

    return invalid;
}

uint16_t Blobs::combine2(uint16_t *boxes, uint16_t numBoxes)
{
    uint16_t i, j, ii, jj, left0, right0, top0, bottom0;
    uint16_t left, right, top, bottom, height, width;
    uint16_t invalid;

    // delete blobs that are fully enclosed by larger blobs
    for (i=0, ii=0, invalid=0; i<numBoxes; i++, ii+=5)
    {
        if (boxes[ii+0]==0)
            continue;
        left0 = boxes[ii+1];
        right0 = boxes[ii+2];
        top0 = boxes[ii+3];
        bottom0 = boxes[ii+4];

        for (j=i+1, jj=ii+5; j<numBoxes; j++, jj+=5)
        {
            if (boxes[jj+0]==0)
                continue;
            left = boxes[jj+1];
            right = boxes[jj+2];
            top = boxes[jj+3];
            bottom = boxes[jj+4];

            width = (right-left)/2;
            height = (bottom-top)/2;

#if 1 // if corners touch....
            if (left<=left0 && left0-right<=m_mergeDist &&
                    ((top0<=top && top<=bottom0) || (top0<=bottom && bottom<=bottom0)))
            {
                boxes[ii+1] = left;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (right>=right0 && left-right0<=m_mergeDist &&
                    ((top0<=top && top<=bottom0) || (top0<=bottom && bottom<=bottom0)))
            {
                boxes[ii+2] = right;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (top<=top0 && top0-bottom<=m_mergeDist &&
                    ((left0<=left && left<=right0) || (left0<=right && right<=right0)))
            {
                boxes[ii+3] = top;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (bottom>=bottom0 && top-bottom0<=m_mergeDist &&
                    ((left0<=left && left<=right0) || (left0<=right && right<=right0)))
            {
                boxes[ii+4] = bottom;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
#else // at least half of a side (the smaller adjacent side) has to overlap
            if (left<=left0 && left0-right<=m_mergeDist &&
                    ((top<=top0 && top0<=top+height) || (top+height<=bottom0 && bottom0<=bottom)))
            {
                boxes[ii+1] = left;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (right>=right0 && left-right0<=m_mergeDist &&
                     ((top<=top0 && top0<=top+height) || (top+height<=bottom0 && bottom0<=bottom)))
            {
                boxes[ii+2] = right;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (top<=top0 && top0-bottom<=m_mergeDist &&
                     ((left<=left0 && left0<=left+width) || (left+width<=right0 && right0<=right)))
            {
                boxes[ii+3] = top;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (bottom>=bottom0 && top-bottom0<=m_mergeDist &&
                     ((left<=left0 && left0<=left+width) || (left+width<=right0 && right0<=right)))
            {
                boxes[ii+4] = bottom;
                boxes[jj+0] = 0; // invalidate
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

    left0 = m_boxes[a*4+0];
    if (left0==0)
        return false;
    model0 = left0&0x07;
    left0 >>= 3;
    right0 = m_boxes[a*4+1]>>3;
    top0 = m_boxes[a*4+2]>>3;
    bottom0 = m_boxes[a*4+3]>>3;

    left = m_boxes[b*4+0];
    if (left==0)
        return false;
    model = left&0x07;
    if (model0==model)
        return false;
    left >>= 3;
    right = m_boxes[b*4+1]>>3;
    top = m_boxes[b*4+2]>>3;
    bottom = m_boxes[b*4+3]>>3;

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

    model = m_boxes[a*4]&0x07;
    codedModel = model;
    codedModel <<= 3;
    model = m_boxes[b*4]&0x07;
    codedModel |= model;
    codedModel <<= 3; // 0 escape

    left = m_boxes[a*4+0]>>3 < m_boxes[b*4+0]>>3 ? m_boxes[a*4+0]>>3 : m_boxes[b*4+0]>>3;
    right = m_boxes[a*4+1]>>3 > m_boxes[b*4+1]>>3 ? m_boxes[a*4+1]>>3 : m_boxes[b*4+1]>>3;
    top = m_boxes[a*4+2]>>3 < m_boxes[b*4+2]>>3 ? m_boxes[a*4+2]>>3 : m_boxes[b*4+2]>>3;
    bottom = m_boxes[a*4+3]>>3 > m_boxes[b*4+3]>>3 ? m_boxes[a*4+3]>>3 : m_boxes[b*4+3]>>3;

#ifdef RENDER_ANGLE
    // calculate angle
    int acx = ((m_boxes[a*4+1]>>3)+(m_boxes[a*4+0]>>3))/2;
    int acy = ((m_boxes[a*4+3]>>3)+(m_boxes[a*4+2]>>3))/2;
    int bcx = ((m_boxes[b*4+1]>>3)+(m_boxes[b*4+0]>>3))/2;
    int bcy = ((m_boxes[b*4+3]>>3)+(m_boxes[b*4+2]>>3))/2;
    double angle = atan2(acy-bcy, acx-bcx)*180/3.1415;
    //qDebug() << "angle " << angle << " acx " << acx << " acy " << acy << " bcx " << bcx << " bcy " << bcy;

    // add rectangle
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+0] = codedModel;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+1] = left;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+2] = right;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+3] = top;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+4] = bottom;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+5] = (int16_t)angle;
#else
    // add rectangle
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+0] = codedModel;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+1] = left;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+2] = right;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+3] = top;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+4] = bottom;
#endif

    m_numCodedBoxes++;

    // invalidate a and b (only if you are
    m_boxes[a*4+0] = 0;
    m_boxes[a*4+1] = 0;
    m_boxes[a*4+2] = 0;
    m_boxes[a*4+3] = 0;
    m_boxes[b*4+0] = 0;
    m_boxes[b*4+1] = 0;
    m_boxes[b*4+2] = 0;
    m_boxes[b*4+3] = 0;
}

void Blobs::processCoded()
{
    int i, j;

    m_numCodedBoxes = 0;
    for (i=0; i<m_numBoxes; i++)
    {
        for (j=i+1; j<m_numBoxes; j++)
        {
            if (closeby(i, j, MAX_CODED_DIST))
                addCoded(i, j);
        }
    }
}

